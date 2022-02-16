import Entities.PersonBase
import Entities.Employe
import Entities.Tenant
import Entities.Hotel
import Entities.Room

import Data.Time.Calendar
import Data.Time

main :: IO () 
main = do
    let s = PersonBase "123" "123" "123" (fromGregorian 2018 10 27)
    print (show (birthDay s))

