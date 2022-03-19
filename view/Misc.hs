{-# LANGUAGE OverloadedStrings, OverloadedLabels, ScopedTypeVariables, LambdaCase, InstanceSigs #-}

module Misc where

import qualified Data.Text.IO as Console
import Data.Text (Text)

import qualified GI.Gtk as Gtk
import Data.GI.Base

data UserView = UserView {
    firstName   :: String,
    secondName  :: String,
    phoneNumer  :: String
} deriving (Show)

data ProfitView = ProfitView {
    profitType   :: String,
    roomNumber   :: String,
    date         :: String,
    summ         :: Double
} deriving (Show)

data BookingView = BookingView {
    firstName_   :: String,
    lastName     :: String,
    phoneNumber_ :: String,
    dates        :: String 
} deriving (Show)

getBuilderObj :: forall o'
               . GObject o' 
               => Gtk.Builder 
               -> Text 
               -> (ManagedPtr o' -> o') 
               -> IO (Maybe o')
getBuilderObj builder name gtkConstr = #getObject builder name >>= \case 
  Just obj -> castTo gtkConstr obj
  Nothing -> do
    Console.putStrLn $ "Object named '" <> name <> "' could not be found."
    return Nothing

