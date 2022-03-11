module Entities.Room where

import Entities.RoomComfortItem
import Entities.Employe
import Entities.Tenant

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
