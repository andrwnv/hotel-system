{-# LANGUAGE OverloadedStrings, OverloadedLabels, ScopedTypeVariables, LambdaCase, InstanceSigs #-}

import qualified Data.Text.IO as T
import Data.Text (Text, pack)
import Data.Maybe
import Data.List (sort, concat)
import System.Environment (getArgs)

import qualified GI.Gtk as Gtk
import Data.GI.Base

import Core.Utils.DayChecks


printQuit :: Text -> IO ()
printQuit t = do
  T.putStrLn $ "Quitting by " <> t <> "."
  Gtk.mainQuit
  return ()

getBuilderObj :: forall o'
               . GObject o' 
               => Gtk.Builder 
               -> Text 
               -> (ManagedPtr o' -> o') 
               -> IO (Maybe o')
getBuilderObj builder name gtkConstr = #getObject builder name >>= \case 
  Just obj -> castTo gtkConstr obj
  Nothing -> do
    T.putStrLn $ "Object named '" <> name <> "' could not be found."
    return Nothing

-- Be aware that this function silently ignores absent names
connectBtnClick :: Gtk.Builder -> Text -> IO () -> IO ()
connectBtnClick builder name handler = getBuilderObj builder name Gtk.Button >>= \case
  Just button -> do 
    on button #clicked $ do handler
    return ()
  Nothing -> return ()

printHello :: Gtk.Builder -> Text -> IO ()
printHello builder t = do
  -- Getting text from Gtk.Entry
  Just entry <- getBuilderObj builder "testField" Gtk.Entry
  buffer :: Gtk.EntryBuffer <- #getBuffer entry
  text :: Text <- #getText buffer
  
  T.putStrLn $ "Hello from " <> text <> "."

showSelectedColumn :: Gtk.Builder -> IO ()
printHi builder = do
  -- Getting text from selected item in Gtk.TreeView
  Just entry <- getBuilderObj builder "tree" Gtk.TreeView
  selection :: Gtk.TreeSelection <- #getSelection entry
  treeModel :: (Bool, Gtk.TreeModel, Gtk.TreeIter) <- #getSelected selection

  let (a, b, c) = treeModel
  -- test1 <- #getStringFromIter b c
  test :: Gtk.GValue <- #getValue b c 1
  t :: (Maybe String) <- fromGValue test
  
  let txt :: String = fromJust t
  print (show txt)

  T.putStrLn $ "Hello from "

main :: IO ()
main = do
  args <- getArgs
  let targs = map pack args

  Gtk.init $ Just targs

  let filename = case targs of
                   [] -> "G:\\DEVELOP\\hotel\\src\\main.glade"
                   arg:[] -> arg
                   _ -> error "Too many command line arguments."
  T.putStrLn $ "filename=\"" <> filename <> "\""

  builder <- new Gtk.Builder []
  #addFromFile builder filename

  Just window <- getBuilderObj builder "window" Gtk.Window
  on window #destroy $ printQuit "windows close button"

  let name = "rent"
  connectBtnClick builder name $ do showSelectedColumn builder
  
  currentDay <- now
  print (show currentDay)

  Gtk.main
