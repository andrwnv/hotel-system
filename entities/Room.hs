module Room where

import RoomComfortItem
import Tenant

import Data.Time


data Room = Room {
    roomNumber  :: Int,
    description :: String,
    price       :: Double,
    comforts    :: [RoomComfortItem],
    dayExpenses :: Double,

    busyTime    :: [Day],
    busyBy      :: [Tenant]
} deriving (Show, Read)
