{-# LANGUAGE OverloadedStrings, OverloadedLabels, ScopedTypeVariables, LambdaCase, InstanceSigs #-}

module RentCore (selectedDaysBusy) where

import DayChecks

import Tenant
import Room
import Data.Time

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
