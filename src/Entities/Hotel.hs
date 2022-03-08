module Entities.Hotel where

import Entities.VacationItem
import Entities.HistoryItem
import Entities.Employe
import Entities.Tenant
import Entities.Room 

data Hotel = Hotel {
    employees       :: [Employe],
    tenants         :: [Tenant],
    rooms           :: [Room],
    
    history         :: [HistoryItem],
    vacationList    :: [VacationItem]
} deriving (Show, Read)
