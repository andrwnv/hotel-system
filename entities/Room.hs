module Room where

import Tenant

import Data.Time

type Rent = (Tenant, [Day]) 

data Room = Room {
    roomNumber   :: Int,
    description  :: String,
    tenantPrice  :: Double,
    dayExpenses  :: Double,
    busyTime     :: [Day],
    busyBy       :: [Tenant],
    plannedRents :: [Rent]
} deriving (Show, Read)
