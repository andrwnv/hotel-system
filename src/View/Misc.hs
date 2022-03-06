{-# LANGUAGE OverloadedStrings, OverloadedLabels, ScopedTypeVariables, LambdaCase, InstanceSigs #-}

module View.Misc where

import qualified Data.Text.IO as Console
import Data.Text (Text)

import qualified GI.Gtk as Gtk
import Data.GI.Base


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

