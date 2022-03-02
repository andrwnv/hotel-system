module Entities.Employe where

import Entities.PersonBase
import Data.Time

data Employe = Employe {
    base            :: PersonBase,

    salaryPerHour   :: Double,
    hourPerWeek     :: Int,
    
    startDate       :: Day,
    positionName    :: String,
    email           :: String
}
