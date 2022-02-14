module Entities.Hotel where

import Entities.Person 
import Entities.Room 

data Hotel = Hotel {
    personal :: [Person],
    rooms :: [Room]
}
