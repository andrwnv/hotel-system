{-# 
    LANGUAGE OverloadedStrings, 
    OverloadedLabels, ScopedTypeVariables, 
    LambdaCase, InstanceSigs
#-}

module Mutation where

import Data.Text (Text)
import Data.Maybe
import Data.Int

import qualified GI.Gtk as Gtk
import Data.GI.Base

import Misc


addRowToProfitView :: Gtk.Builder -> Text -> ProfitView -> IO ()
addRowToProfitView builder listStoreId profit = do
    Just store <- getBuilderObj builder listStoreId Gtk.ListStore

    _profitType <- toGValue $ Just $ profitType profit
    _roomNumber <- toGValue $ Just $ roomNumber profit
    _date <- toGValue $ Just $ date profit
    _sum <- toGValue $ summ profit

    newIter :: Gtk.TreeIter <- #append store
    #setValue store newIter 0 _profitType
    #setValue store newIter 1 _roomNumber
    #setValue store newIter 2 _date
    #setValue store newIter 3 _sum

addRowToUserView :: Gtk.Builder -> Text -> UserView -> IO ()
addRowToUserView builder listStoreId user = do
    Just store <- getBuilderObj builder listStoreId Gtk.ListStore

    _firstName <- toGValue $ Just $ firstName user
    _lastName <- toGValue $ Just $ secondName user
    _phoneNum <- toGValue $ Just $ phoneNumer user

    newIter :: Gtk.TreeIter <- #append store
    #setValue store newIter 0 _firstName
    #setValue store newIter 1 _lastName
    #setValue store newIter 2 _phoneNum

addRowToBookingView :: Gtk.Builder -> Text -> BookingView -> IO ()
addRowToBookingView builder listStoreId booking = do
    Just store <- getBuilderObj builder listStoreId Gtk.ListStore

    _firstName <- toGValue $ Just $ firstName_ booking
    _lastName <- toGValue $ Just $ lastName booking
    _phoneNum <- toGValue $ Just $ phoneNumber_ booking
    _dates <- toGValue $ Just $ dates booking
    print $ show booking
    newIter :: Gtk.TreeIter <- #append store
    #setValue store newIter 0 _lastName
    #setValue store newIter 1 _firstName
    #setValue store newIter 2 _phoneNum
    #setValue store newIter 3 _dates

clearTreeView :: Gtk.Builder -> Text -> IO ()
clearTreeView builder listStoreId = do
    Just store <- getBuilderObj builder listStoreId Gtk.ListStore
    #clear store

addComboBoxItem :: Gtk.Builder -> Text -> Text -> IO ()
addComboBoxItem builder comboBoxId text = do
    Just comboBox <- getBuilderObj builder comboBoxId Gtk.ComboBoxText
    #appendText comboBox text

setActiveComboBoxItem :: Gtk.Builder -> Text -> Int32 -> IO ()
setActiveComboBoxItem builder comboBoxId index = do
    Just comboBox <- getBuilderObj builder comboBoxId Gtk.ComboBoxText
    #setActive comboBox index

setSensetiveWidget :: Gtk.Builder -> Text -> Bool -> IO ()
setSensetiveWidget builder id state = do
    Just widget <- getBuilderObj builder id Gtk.Widget
    #setSensitive widget state

changeLabelText :: Gtk.Builder -> Text -> Text -> IO ()
changeLabelText builder labelId text = do
    Just label <- getBuilderObj builder labelId Gtk.Label
    #setText label text
