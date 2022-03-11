module Hotel where

import HistoryItem
import Tenant
import Room 

data Hotel = Hotel {
    tenants         :: [Tenant],
    rooms           :: [Room],
    history         :: [HistoryItem]
} deriving (Show, Read)
