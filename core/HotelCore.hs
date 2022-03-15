{-# LANGUAGE OverloadedStrings, OverloadedLabels, ScopedTypeVariables, LambdaCase, InstanceSigs #-}

module HotelCore (collectProfit) where

import Data.Time.Calendar
import GHC.Float

import RoomComfortItem
import HistoryItem
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
