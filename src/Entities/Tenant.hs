module Entities.Tenant where

import Entities.PersonBase

data Tenant = Tenant {
    base        :: PersonBase,
    email       :: String,
    roomNumber  :: Int
} deriving (Show, Read)
