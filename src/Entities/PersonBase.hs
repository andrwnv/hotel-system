module Entities.PersonBase where

import Data.Time

data PersonBase = PersonBase {
    firstName   :: String,
    lastName    :: String,
    phoneNumer  :: String,
    birthDay    :: Day
}
