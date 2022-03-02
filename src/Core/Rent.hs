module Core.Rent where

import Core.Utils.DayChecks

import Entities.Tenant
import Entities.Room
import Data.Time

data RentType = Room | Bool

-- rent :: Room -> [Tenant] -> [Day] -> Room
-- rent selectedRoom tenants selectedDays = 
--     let isCorrect = do
--                     _isCorrectDays <- isCorrectDatePair selectedDays
--     return selectedRoom
