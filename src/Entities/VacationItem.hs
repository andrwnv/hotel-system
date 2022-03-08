module Entities.VacationItem where

import Entities.Employe

import Data.Time

data VacationItem = VacationItem {
    employe :: Employe,
    dates   :: [Day]
} deriving (Show, Read)
