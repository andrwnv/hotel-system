module Entities.RoomComfortItem where

data RoomComfortItem = RoomComfortItem {
    price       :: Double,
    description :: String,
    disposable  :: Bool
} deriving (Show, Read)
