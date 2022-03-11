module Tenant where

import PersonBase

data Tenant = Tenant {
    base        :: PersonBase,
    email       :: String,
    roomNumber  :: Int
} deriving (Show, Read)
