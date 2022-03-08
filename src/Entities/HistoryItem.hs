module Entities.HistoryItem where

import Entities.Tenant
import Entities.Room

import Data.Time

data HistoryItem = HistoryItem {
    room            :: Room,
    tenant          :: Tenant,
    totalPrice      :: Double,
    reservationDate :: Day
} deriving (Show, Read)
