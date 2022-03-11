{-# 
    LANGUAGE OverloadedStrings, 
    OverloadedLabels, ScopedTypeVariables, 
    LambdaCase, InstanceSigs
#-}

module Mutation where

import Data.Text (Text)
import Data.Maybe

import qualified GI.Gtk as Gtk
import Data.GI.Base

import Misc


data ProfitView = ProfitView {
    profitType   :: String,
    roomNumber   :: String,
    date         :: String,
    summ         :: Double
} deriving (Show)


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

addComboBoxItem :: Gtk.Builder -> Text -> Text -> IO ()
addComboBoxItem builder comboBoxId text = do
    Just comboBox <- getBuilderObj builder comboBoxId Gtk.ComboBoxText
    #appendText comboBox text

changeLabelText :: Gtk.Builder -> Text -> Text -> IO ()
changeLabelText builder labelId text = do
    Just label <- getBuilderObj builder labelId Gtk.Label
    #setText label text
