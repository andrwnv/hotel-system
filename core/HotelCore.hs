{-# LANGUAGE OverloadedStrings, OverloadedLabels, ScopedTypeVariables, LambdaCase, InstanceSigs #-}

module HotelCore (collectProfit, replaceRoom) where

import Data.Time.Calendar
import GHC.Float

import RoomComfortItem
import HistoryItem
import Hotel
import Room

_roomComfortProfit :: [RoomComfortItem] -> Double
_roomComfortProfit [] = 0.0
_roomComfortProfit (x:xs) = comfortPrice + _roomComfortProfit xs
    where
        comfortPrice = price x

collectProfit :: [HistoryItem] -> (Double, Double)
collectProfit [] = (0.0, 0.0)
collectProfit (x:xs) = (totalProfit, totalCost)
    where
        currProfitInfo = collectProfit xs
        dates = reservationDate x
        roomCost = dayExpenses (room x)
        daysDiff = fromIntegral $ ((diffDays (dates!!1) (dates!!0)) + 1)
        totalProfit = (fst currProfitInfo) + (totalPrice x)
        totalCost = (snd currProfitInfo) + (daysDiff * roomCost)


_roomsWithoutSelectedRoom :: Int -> [Room] -> [Room]
_roomsWithoutSelectedRoom _ [] = []
_roomsWithoutSelectedRoom roomNum (x:xs)
    | roomNum == roomNumber x = _roomsWithoutSelectedRoom roomNum xs
    | otherwise = [x] ++ _roomsWithoutSelectedRoom roomNum xs

replaceRoom :: Hotel -> Room -> Hotel
replaceRoom hotel newRoom = hotelRes
    where 
        _tenants = tenants hotel
        _rooms = [newRoom] ++ _roomsWithoutSelectedRoom (roomNumber newRoom) (rooms hotel)
        _history = history hotel
        hotelRes = Hotel _tenants _rooms _history
