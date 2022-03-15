{-# LANGUAGE OverloadedStrings, OverloadedLabels, ScopedTypeVariables, LambdaCase, InstanceSigs #-}

import qualified Data.Text.IO as T
import Data.Text (Text, pack)
import Data.Maybe
import Data.List (sort, concat)
import System.Environment (getArgs)

import Data.IORef
import Control.Monad

import qualified GI.Gtk as Gtk
import Data.GI.Base

import DayChecks

import Extractors
import Connectors
import Mutation
import Misc
import RentCore

import Data.Time.Calendar
import PersonBase
import Tenant

import UserCore

import Hotel
import Room
import RoomComfortItem
import HistoryItem

import Combiner

import qualified ViewID as ID

roomComboBox_ID :: Text = "roomComboBox"

printQuit :: Text -> IO ()
printQuit t = do
  T.putStrLn $ "Quitting by " <> t <> "."
  Gtk.mainQuit
  return ()

showSelectedColumn :: Gtk.Builder -> IO ()
showSelectedColumn builder = do
  -- Getting text from selected item in Gtk.TreeView
  value <- extractSelectedRow_User builder "tree"
  let t = fromJust value
  print (show (t))
  T.putStrLn $ "Hello from "

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

  Just window <- getBuilderObj builder "window" Gtk.Window
  on window #destroy $ printQuit "windows close button"

  let name = "rent"
  connectButtonClicked builder name $ do showSelectedColumn builder

  let test = [Tenant (PersonBase "123" "123" "123" (fromGregorian 2022 03 08)) "" (-1), Tenant (PersonBase "123" "123" "123" (fromGregorian 2022 03 08)) "" (-1)]
  let room = Room 1 "123" [RoomComfortItem 12.0 "456" True] 100.29 [] [] []

  let item = HistoryItem "MasterCard" room 1235.50 (fromGregorian 2022 03 08)

  hotelGlobalInstance <- newIORef $ Hotel [] [room] [item, item, item]

  connectButtonClicked builder (ID.create_createUserBtnId) $ createUserHandler builder hotelGlobalInstance
  connectButtonClicked builder (ID.delete_deleteUserBtnId) $ deleteUserHandler builder hotelGlobalInstance

  loadProfitTable builder hotelGlobalInstance

  -- value <- extractSelectedRow_User builder "profitTree"
  -- let t = fromJust value 
  -- print (show (t))
  
  -- selectedDate <- extractDate builder "birthDayCal"
 
  -- Just txtSelect <- extractComboBoxText builder roomComboBox_ID
  -- print(show(txtSelect))
  
  -- addComboBoxItem builder "roomComboBox" "test123132"

  -- changeLabelText builder "totalProfitLabel" "100000,00 р."

  -- let tenant = Tenant (PersonBase "123" "123" "123" (fromGregorian 2022 03 08)) "" (-1)
  -- let person = PersonBase "1234" "123" "123" (fromGregorian 2022 03 08)

  -- print $ isUserExist [tenant] person

  -- let select = [(fromGregorian 2022 03 10), (fromGregorian 2022 03 18)]
  -- let begin = (fromGregorian 2022 03 11)
  -- let end = (fromGregorian 2022 03 20)
 
  -- let selectBegin = select!!0
  -- let selectEnd = select!!1
  -- let rentBegin = begin
  -- let rentEnd = end

  -- let rent = [([tenant], [begin, end]), ([tenant], [(fromGregorian 2022 03 21), (fromGregorian 2022 03 25)])]
  -- print $ selectedDaysBusy rent select

  -- -- writeFile "file.txt" (show test)
  -- contents :: String <- readFile "file.txt"
  -- let arr :: [Tenant] = read contents

  -- print $ show arr

  Gtk.main
