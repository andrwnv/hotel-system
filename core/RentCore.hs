{-# LANGUAGE OverloadedStrings, OverloadedLabels, ScopedTypeVariables, LambdaCase, InstanceSigs #-}

module RentCore (selectedDaysBusy, deleteBooking) where

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

-- rent :: Room -> [Tenant] -> [Day] -> IO (Maybe Room)
-- rent selectedRoom tenants selectedDays = 
--     isCorrectDays <- isCorrectDatePair selectedDays
--     return selectedRoom

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
