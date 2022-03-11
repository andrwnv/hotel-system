module HistoryItem where

import Tenant
import Room

import Data.Time

data HistoryItem = HistoryItem {
    room            :: Room,
    tenant          :: Tenant,
    totalPrice      :: Double,
    reservationDate :: Day
} deriving (Show, Read)
