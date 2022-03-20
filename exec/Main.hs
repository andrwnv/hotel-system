{-# LANGUAGE OverloadedStrings, OverloadedLabels, ScopedTypeVariables, LambdaCase, InstanceSigs #-}
{-# OPTIONS_GHC -w #-}

import qualified Data.Text.IO as T
import Data.IORef
import Data.Text (Text, pack)
import Data.Time.Calendar
import System.Environment (getArgs)

import Control.Monad

import qualified GI.Gtk as Gtk
import Data.GI.Base

import qualified Extractors
import qualified Connectors
import qualified Misc

import HistoryItem
import PersonBase
import HotelCore
import Tenant
import Hotel
import Room

import Combiner

import qualified ViewID as ID

printQuit :: Text -> IO ()
printQuit t = do
  T.putStrLn $ "Quitting by " <> t <> "."
  Gtk.mainQuit
  return ()

main :: IO ()
main = do
  args <- getArgs
  let targs = map pack args

  Gtk.init $ Just targs

  let filename = case targs of
                   [] -> "G:\\DEVELOP\\hotel\\exec\\main.glade"
                   arg:[] -> arg
                   _ -> error "Too many command line arguments."
  T.putStrLn $ "filename=\"" <> filename <> "\""

  builder <- new Gtk.Builder []
  #addFromFile builder filename

  Just window <- Misc.getBuilderObj builder "window" Gtk.Window
  on window #destroy $ printQuit "windows close button"

  let test = [Tenant (PersonBase "123" "123" "123" (fromGregorian 2022 03 08)) "" (-1), Tenant (PersonBase "123" "123" "123" (fromGregorian 2022 03 08)) "" (-1)]
  let room = Room 1 "123" 5000.0 100.29 [] test []
  let roomt = Room 2 "123" 5000.0 100.29 [] test []

  let item = HistoryItem "MasterCard" room 40000.0 [(fromGregorian 2022 03 01), (fromGregorian 2022 03 08)]

  let users = [Tenant (PersonBase "Glazunov" "Andrew" "89521515969" (fromGregorian 2022 03 08)) "" (-1), 
              Tenant (PersonBase "QWerty" "Petr" "89521325969"  (fromGregorian 2022 03 08)) "" 3,
              Tenant (PersonBase "QWerty123" "Petr" "89521325967"  (fromGregorian 2022 03 08)) "" (-1)]

  let room3Users = [Tenant (PersonBase "QWerty" "Petr" "89521325969"  (fromGregorian 2022 03 08)) "" 3]
  let testRent = ((Tenant (PersonBase "QWerty" "Petrov" "89521325969"  (fromGregorian 2022 03 08)) "" (-1)), [(fromGregorian 2022 03 20), (fromGregorian 2022 03 30)])

  let datesPair = [(fromGregorian 2022 03 01), (fromGregorian 2022 03 07)]
  let roomt2 = Room 3 "qwerty123123" 5000.0 100.29 datesPair room3Users [testRent]

  hotelGlobalInstance <- newIORef $ Hotel users [room, roomt, roomt2] [item, item, item]

  Connectors.connectButtonClicked builder ID.create_createUserBtnId $ createUserHandler builder hotelGlobalInstance
  Connectors.connectButtonClicked builder ID.delete_deleteUserBtnId $ deleteUserHandler builder hotelGlobalInstance
  Connectors.connectButtonClicked builder ID.booking_deleteBtnId $ Combiner.deleteBooking builder hotelGlobalInstance

  Connectors.connectComboBoxTextSelect builder ID.room_roomComboBoxID (loadRoomInfo builder hotelGlobalInstance)

  -- Evict from room
  Just evictDialog <- Misc.getBuilderObj builder ID.evict_dialogId Gtk.Dialog

  Connectors.connectButtonClicked builder ID.users_leaveBtnId $ riseEvictDialog builder hotelGlobalInstance
  Connectors.connectButtonClicked builder ID.evict_cancelBtnId $ evictFromDialogCancel evictDialog
  Connectors.connectButtonClicked builder ID.evict_evictBtnId $ evictDialogHandler builder evictDialog hotelGlobalInstance

  -- Loading 
  loadProfitTable builder hotelGlobalInstance
  loadRooms builder hotelGlobalInstance

  -- let rent = [([tenant], [begin, end]), ([tenant], [(fromGregorian 2022 03 21), (fromGregorian 2022 03 25)])]
  -- -- writeFile "file.txt" (show test)
  -- contents :: String <- readFile "file.txt"
  -- let arr :: [Tenant] = read contents

  Gtk.main
