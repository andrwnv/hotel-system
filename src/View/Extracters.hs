{-# LANGUAGE OverloadedStrings, OverloadedLabels, ScopedTypeVariables, LambdaCase, InstanceSigs #-}

module View.Extracters where

import Control.Monad
import Data.Time.Format
import Data.Text (Text)
import Data.Maybe
import Data.Time

import qualified GI.Gtk as Gtk
import Data.GI.Base

import View.Misc as Misc


data UserView = UserView {
    firstName   :: String,
    secondName  :: String,
    phoneNumer  :: String
} deriving (Show)


extractEntryText :: Gtk.Builder -> Text -> IO Text
extractEntryText builder entryId = do
    Just entry <- Misc.getBuilderObj builder entryId Gtk.Entry
    buffer :: Gtk.EntryBuffer <- #getBuffer entry
    text :: Text <- #getText buffer
    return text

extractSelectedRow_User :: Gtk.Builder -> Text -> IO (Maybe UserView)
extractSelectedRow_User builder treeViewId = do
    Just treeView <- getBuilderObj builder treeViewId Gtk.TreeView
    selection :: Gtk.TreeSelection <- #getSelection treeView
    treeModel :: (Bool, Gtk.TreeModel, Gtk.TreeIter) <- #getSelected selection

    let (selected, model, iter) = treeModel
    when (not selected) (return ())

    -- extract firstName, secondName & phoneNumber
    gtkValue :: Gtk.GValue <- #getValue model iter 0
    value :: (Maybe String) <- fromGValue gtkValue
    let firstName = fromMaybe "" value

    gtkValue :: Gtk.GValue <- #getValue model iter 1
    value :: (Maybe String) <- fromGValue gtkValue
    let lastName = fromMaybe "" value

    gtkValue :: Gtk.GValue <- #getValue model iter 1
    value :: (Maybe String) <- fromGValue gtkValue
    let phoneNumber = fromMaybe "" value

    let result = Just (UserView firstName lastName phoneNumber)

    return result 

-- extractSelectedRow_Profit :: Gtk.Builder -> Text -> IO (Maybe ProfitView)
-- extractSelectedRow_Profit builder treeViewId = do
--     Just treeView <- getBuilderObj builder treeViewId Gtk.TreeView
--     selection :: Gtk.TreeSelection <- #getSelection treeView
--     treeModel :: (Bool, Gtk.TreeModel, Gtk.TreeIter) <- #getSelected selection

--     let (selected, model, iter) = treeModel
--     when (not selected) (return ())

--     -- extract profitType, roomNumber, date & phoneNumber
--     gtkValue :: Gtk.GValue <- #getValue model iter 0
--     value :: (Maybe String) <- fromGValue gtkValue
--     let profitType = fromMaybe "" value

--     gtkValue :: Gtk.GValue <- #getValue model iter 1
--     value :: (Maybe String) <- fromGValue gtkValue
--     let roomNumber = fromMaybe "" value

--     gtkValue :: Gtk.GValue <- #getValue model iter 1
--     value :: (Maybe String) <- fromGValue gtkValue
--     let dateString = fromMaybe "" value
--     let utcDateTime :: UTCTime = parseTimeOrError True defaultTimeLocale "%d-%m-%Y" dateString
--     let (year, month, day) = toGregorian $ utctDay utcDateTime
--     let date = (fromGregorian year month day)

--     gtkValue :: Gtk.GValue <- #getValue model iter 1
--     value :: Double <- fromGValue gtkValue
--     let sum = value

--     let result = Just (ProfitView profitType roomNumber date sum)

--     return result 

