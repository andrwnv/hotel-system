{-# LANGUAGE OverloadedStrings, OverloadedLabels, ScopedTypeVariables, LambdaCase, InstanceSigs #-}

module RentCore (selectedDaysBusy
                , deleteBooking
                , rent
                , moveIn) where

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

_createBooking :: Tenant -> Room -> [Day] -> Room
_createBooking person room dates = res
    where
        _roomNumber   = Room.roomNumber room
        _description  = description room
        _tenantPrice  = tenantPrice room
        _dayExpenses  = dayExpenses room
        _busyTime     = busyTime room
        _busyBy       = busyBy room
        _plannedRents = [(person, dates)] ++ plannedRents room
        res = Room _roomNumber _description _tenantPrice _dayExpenses _busyTime _busyBy _plannedRents

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
                        False -> return $ Just $ _createBooking person room dates
                        _ -> do
                            let _roomNumber   = Room.roomNumber room
                            let _description  = description room
                            let _tenantPrice  = tenantPrice room
                            let _dayExpenses  = dayExpenses room
                            let _busyTime     = dates
                            let _busyBy       = [person]
                            let _plannedRents = plannedRents room
                            return $ Just $ Room _roomNumber _description _tenantPrice _dayExpenses _busyTime _busyBy _plannedRents

moveIn :: Tenant -> Room -> Room
moveIn person room = room


