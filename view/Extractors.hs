{-# 
    LANGUAGE OverloadedStrings, 
    OverloadedLabels, ScopedTypeVariables, 
    LambdaCase, InstanceSigs
#-}

module Extractors where

import Control.Monad

import Data.Time.Calendar
import Data.Time.Format
import Data.Text (Text)
import Data.Maybe
import Data.Time
import Data.Int


import qualified GI.Gtk as Gtk
import Data.GI.Base

import Misc


data UserView = UserView {
    firstName   :: String,
    secondName  :: String,
    phoneNumer  :: String
} deriving (Show)


-- Gtk.Entry extract user input
extractEntryText :: Gtk.Builder -> Text -> IO Text
extractEntryText builder entryId = do
    Just entry <- getBuilderObj builder entryId Gtk.Entry
    buffer :: Gtk.EntryBuffer <- #getBuffer entry
    text :: Text <- #getText buffer
    return text

-- Gtk.TreeView extract user treeView selection
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

    gtkValue :: Gtk.GValue <- #getValue model iter 2
    value :: (Maybe String) <- fromGValue gtkValue
    let phoneNumber = fromMaybe "" value

    let result = Just (UserView firstName lastName phoneNumber)

    return result 

-- Gtk.Calendar extract user date selection
extractDate :: Gtk.Builder -> Text -> IO Day
extractDate builder calendarId = do
    Just calendar <- getBuilderObj builder calendarId Gtk.Calendar
    (yyyy, mm, dd) <- Gtk.calendarGetDate calendar

    return $ fromGregorian (fromIntegral yyyy) (fromIntegral mm + 1) (fromIntegral dd)

-- Gtk.ComboBoxText extract user comboBox selection
extractComboBoxText :: Gtk.Builder -> Text -> IO (Maybe Text)
extractComboBoxText builder comboBoxId = do
    Just comboBox <- getBuilderObj builder comboBoxId Gtk.ComboBoxText
    selectedItem <- #getActiveText comboBox

    return selectedItem
