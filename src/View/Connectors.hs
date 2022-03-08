{-# 
    LANGUAGE OverloadedStrings, 
    OverloadedLabels, ScopedTypeVariables, 
    LambdaCase, InstanceSigs 
#-}

module View.Connectors where

import Data.Text (Text)

import qualified GI.Gtk as Gtk
import Data.GI.Base

import View.Misc as Misc

connectButtonClicked :: Gtk.Builder -> Text -> IO () -> IO ()
connectButtonClicked builder buttonId handler = Misc.getBuilderObj builder buttonId Gtk.Button >>= \case
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

