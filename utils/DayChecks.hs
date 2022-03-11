{-# LANGUAGE ScopedTypeVariables, InstanceSigs #-}

module DayChecks where

import Data.Time
import Data.Time.Calendar
import Data.Dynamic

now :: IO Day
now = do 
    now <- getCurrentTime
    let (year, month, day) = toGregorian $ utctDay now
    let currentDate = (fromGregorian year month day)
    return currentDate

isCorrectDay :: Day -> IO Bool
isCorrectDay day = do
    currentDay :: Day <- now 
    let result = currentDay <= day
    return result

isCorrectDatePair :: [Day] -> IO Bool
isCorrectDatePair dates = do
    let begin = dates!!0 
    let end = dates!!1
    d1 <- isCorrectDay begin
    d2 <- isCorrectDay end
    return (length dates == 2 && begin <= end && d1 && d2)
