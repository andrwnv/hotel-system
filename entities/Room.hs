module Room where

import RoomComfortItem
import Tenant

import Data.Time

data Rent = Tuple ([Tenant], [Day]) deriving (Show, Read)

data Room = Room {
    roomNumber   :: Int,
    description  :: String,
    price        :: Double,
    comforts     :: [RoomComfortItem],
    dayExpenses  :: Double,
    busyTime     :: [Day],
    busyBy       :: [Tenant],
    plannedRents :: [Rent]
} deriving (Show, Read)
