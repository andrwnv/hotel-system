{-# LANGUAGE OverloadedStrings, OverloadedLabels, ScopedTypeVariables, LambdaCase, InstanceSigs #-}

import qualified Data.Text.IO as T
import Data.Text (Text, pack)
import Data.Maybe
import Data.List (sort, concat)
import System.Environment (getArgs)

import qualified GI.Gtk as Gtk
import Data.GI.Base

import DayChecks

import Extractors
import Connectors
import Mutation
import Misc

roomComboBox_ID :: Text = "roomComboBox"

printQuit :: Text -> IO ()
printQuit t = do
  T.putStrLn $ "Quitting by " <> t <> "."
  Gtk.mainQuit
  return ()

printHello :: Gtk.Builder -> Text -> IO ()
printHello builder t = do
  -- Getting text from Gtk.Entry
  Just entry <- getBuilderObj builder "testField" Gtk.Entry
  buffer :: Gtk.EntryBuffer <- #getBuffer entry
  text :: Text <- #getText buffer
  
  T.putStrLn $ "Hello from " <> text <> "."

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
  
  currentDay <- now
  print (show currentDay)

  value <- extractSelectedRow_User builder "profitTree"
  let t = fromJust value
  print (show (t))

  selectedDate <- extractDate builder "birthDayCal"

  Just txtSelect <- extractComboBoxText builder roomComboBox_ID
  print(show(txtSelect))
  
  addComboBoxItem builder "roomComboBox" "test123132"

  changeLabelText builder "totalProfitLabel" "100000,00 р."

  Gtk.main
