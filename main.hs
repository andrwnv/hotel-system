import Entities.Person
import Entities.Room
import Entities.Hotel

main :: IO () 
main = do
    let s = Person "Alina" 12
    print (show (age s) ++ name s)

