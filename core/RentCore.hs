{-# LANGUAGE OverloadedStrings, OverloadedLabels, ScopedTypeVariables, LambdaCase, InstanceSigs #-}

module RentCore (selectedDaysBusy
                , deleteBooking
                , rent ) where

import Data.Time

import PersonBase
import Tenant
import Room

import DayChecks


_isSameDates :: [Day] -> [Day] -> Bool
_isSameDates pair1 pair2 = result
    where
        result = pair1!!0 == pair2!!0 && pair1!!1 == pair2!!1


selectedDaysBusy :: [Rent] -> [Day] -> Bool
selectedDaysBusy [] _ = False
selectedDaysBusy (x:xs) selectedDays 
    | (selectEnd < rentBegin || selectBegin > rentEnd)
        = selectedDaysBusy xs selectedDays
    | otherwise = True
    where
        rentDays :: [Day] = (snd x)
        selectBegin = selectedDays!!0
        selectEnd = selectedDays!!1
        rentBegin = rentDays!!0
        rentEnd = rentDays!!1


_deleteBooking :: PersonBase -> [Rent] -> [Rent]
_deleteBooking _ [] = []
_deleteBooking personBase (x:xs)
    | isBookingForDelete = _deleteBooking personBase xs
    | otherwise = [x] ++ _deleteBooking personBase xs
    where
        userBase = base $ fst x
        isBookingForDelete = (firstName userBase) == (firstName personBase) 
                            && (lastName userBase) == (lastName personBase) 
                            && (phoneNumer userBase) == (phoneNumer personBase)

deleteBooking :: PersonBase -> Room -> Room
deleteBooking personBase room = updatedRoom
    where
        updatedRent = _deleteBooking personBase (plannedRents room)
        updatedRoom = Room (Room.roomNumber room) (description room) (tenantPrice room) (dayExpenses room) (busyTime room) (busyBy room) updatedRent

_isBusyForSelectedDays :: [Day] -> [Day] -> Bool
_isBusyForSelectedDays rentDays selectedDays = (selectEnd < rentBegin || selectBegin > rentEnd) || (_isSameDates rentDays selectedDays)
    where
        selectBegin = selectedDays!!0
        selectEnd = selectedDays!!1
        rentBegin = rentDays!!0
        rentEnd = rentDays!!1

_checkBusyDates :: [Day] -> [Day] -> Bool
_checkBusyDates selectedDays rentedDays
    | length selectedDays == 0 || length rentedDays == 0 = False
    | not (selectedDays!!1 < rentedDays!!0 || selectedDays!!0 > rentedDays!!1) = True
    | otherwise = False

_createBooking :: Tenant -> Room -> [Day] -> Maybe Room
_createBooking person room dates
    | _checkBusyDates dates _busyTime = Nothing
    | otherwise = Just res
    where
        _tenantBase = base person
        _email = email person
        updatedPerson = Tenant _tenantBase _email (-1)

        _roomNumber   = Room.roomNumber room
        _description  = description room
        _tenantPrice  = tenantPrice room
        _dayExpenses  = dayExpenses room
        _busyTime     = busyTime room
        _busyBy       = busyBy room
        _plannedRents = [(updatedPerson, dates)] ++ plannedRents room

        res = Room _roomNumber _description _tenantPrice _dayExpenses _busyTime _busyBy _plannedRents

        isNotBusy = length _busyTime


rent :: Tenant -> Room -> [Day] -> IO (Maybe Room)
rent person room dates = do
    isCorrectDates <- isCorrectDatePair dates
    
    case isCorrectDates of
        False -> return Nothing
        _ -> do
            let isBusyDates = selectedDaysBusy (plannedRents room) dates
            case isBusyDates of
                True -> return Nothing
                _ -> do
                    today <- now
                    let isRoomFree = length (busyTime room) == 0 
                    let isToday = dates!!0 == today
                    case isToday && isRoomFree of 
                        False -> do
                            return $ _createBooking person room dates
                        _ -> do
                            let _base        = Tenant.base person  
                                _email       = Tenant.email person

                            let _roomNumber   = Room.roomNumber room
                            let _description  = description room
                            let _tenantPrice  = tenantPrice room
                            let _dayExpenses  = dayExpenses room
                            let _busyTime     = dates
                            let _busyBy       = [(Tenant.Tenant _base _email _roomNumber)]
                            let _plannedRents = plannedRents room
                            return $ Just $ Room _roomNumber _description _tenantPrice _dayExpenses _busyTime _busyBy _plannedRents

