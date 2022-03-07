{-# 
    LANGUAGE OverloadedStrings, 
    OverloadedLabels, ScopedTypeVariables, 
    LambdaCase, InstanceSigs
#-}

module View.Mutation where

import Data.Text (Text)
import Data.Maybe

import qualified GI.Gtk as Gtk
import Data.GI.Base

import View.Misc as Misc


data ProfitView = ProfitView {
    profitType   :: String,
    roomNumber   :: String,
    date         :: String,
    summ         :: Double
} deriving (Show)

data VacationView = VacationView {
    fullName    :: String,
    startDay    :: String,
    endDay      :: String
} deriving (Show)


addRowToProfitView :: Gtk.Builder -> Text -> ProfitView -> IO ()
addRowToProfitView builder listStoreId profit = do
    Just store <- Misc.getBuilderObj builder listStoreId Gtk.ListStore

    _profitType <- toGValue $ Just $ profitType profit
    _roomNumber <- toGValue $ Just $ roomNumber profit
    _date <- toGValue $ Just $ date profit
    _sum <- toGValue $ summ profit

    newIter :: Gtk.TreeIter <- #append store
    #setValue store newIter 0 _profitType
    #setValue store newIter 1 _roomNumber
    #setValue store newIter 2 _date
    #setValue store newIter 3 _sum

addRowToVacationView :: Gtk.Builder -> Text -> VacationView -> IO ()
addRowToVacationView builder listStoreId vacation = do
    Just store <- Misc.getBuilderObj builder listStoreId Gtk.ListStore

    _fullName <- toGValue $ Just $ fullName vacation
    _startDay <- toGValue $ Just $ startDay vacation
    _endDay <- toGValue $ Just $ endDay vacation

    newIter :: Gtk.TreeIter <- #append store
    #setValue store newIter 0 _fullName
    #setValue store newIter 1 _startDay
    #setValue store newIter 2 _endDay

addComboBoxItem :: Gtk.Builder -> Text -> Text -> IO ()
addComboBoxItem builder comboBoxId text = do
    Just comboBox <- Misc.getBuilderObj builder comboBoxId Gtk.ComboBoxText
    #appendText comboBox text
