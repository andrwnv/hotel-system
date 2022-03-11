{-# 
    LANGUAGE OverloadedStrings, 
    OverloadedLabels, ScopedTypeVariables, 
    LambdaCase, InstanceSigs 
#-}

module Connectors where

import Data.Text (Text)

import qualified GI.Gtk as Gtk
import Data.GI.Base

import Misc

connectButtonClicked :: Gtk.Builder -> Text -> IO () -> IO ()
connectButtonClicked builder buttonId handler = getBuilderObj builder buttonId Gtk.Button >>= \case
  Just button -> do 
    on button #clicked $ do handler
    return ()
  Nothing -> return ()

connectComboBoxTextSelect :: Gtk.Builder -> Text -> IO () -> IO ()
connectComboBoxTextSelect builder comboId handler = getBuilderObj builder comboId Gtk.ComboBoxText >>= \case
  Just comboBox -> do 
    on comboBox #changed $ do handler
    return ()
  Nothing -> return ()

