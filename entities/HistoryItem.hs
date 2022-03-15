module HistoryItem where

import Tenant
import Room

import Data.Time

data HistoryItem = HistoryItem {
    paymentType     :: String,
    room            :: Room,
    totalPrice      :: Double,
    reservationDate :: Day
} deriving (Show, Read)
