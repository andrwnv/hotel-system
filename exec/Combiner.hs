{-# LANGUAGE OverloadedStrings, OverloadedLabels, ScopedTypeVariables, LambdaCase, InstanceSigs #-}

module Combiner where

-- Prelude
import Data.Text (Text, unpack)
import Data.IORef
import Control.Monad

-- Gtk
import qualified GI.Gtk as Gtk

-- Core
import UserCore

-- Entities
import PersonBase
import Tenant
import Hotel

-- View
import Extractors
import qualified ViewID as ID 

-- Utils
import DayChecks


createUserHandler :: Gtk.Builder -> IORef Hotel -> IO ()
createUserHandler uiBuilder hotel = do
    phoneNumber     <- extractEntryText uiBuilder (ID.create_PhoneNumberFieldID)
    firstName       <- extractEntryText uiBuilder (ID.create_FirstNameFieldID)
    lastName        <- extractEntryText uiBuilder (ID.create_LastNameFieldID)
    birthDay        <- extractDate      uiBuilder (ID.create_birthDayCalendarID)
    email           <- extractEntryText uiBuilder (ID.create_EmailFieldID)

    hotelCopy <- readIORef hotel
    let _users = tenants hotelCopy

    let tenantBase = PersonBase (unpack firstName) (unpack lastName) (unpack phoneNumber) birthDay
    let isUserValid = isValidUser tenantBase

    if isUserValid
        then do
            let (users, success) = createUser _users (Tenant tenantBase (unpack email) (-1))
            case success of
                True -> do 
                    let _rooms = rooms hotelCopy
                    let _history = history hotelCopy
                    writeIORef hotel (Hotel users _rooms _history)
                    print $ "[CREATE]: User -> " ++ show tenantBase
                False -> print $ "[CREATE]: User already exists -> " ++ show tenantBase
    else print $ "[CREATE]: Invalid user info -> " ++ show tenantBase


deleteUserHandler :: Gtk.Builder -> IORef Hotel -> IO()
deleteUserHandler uiBuilder hotel = do
    phoneNumber <- extractEntryText uiBuilder (ID.delete_PhoneNumberFieldID)
    firstName   <- extractEntryText uiBuilder (ID.delete_FirstNameFieldID)
    lastName    <- extractEntryText uiBuilder (ID.delete_LastNameFieldID)

    hotelCopy <- readIORef hotel
    
    date <- now
    let baseForDelete = PersonBase (unpack firstName) (unpack lastName) (unpack phoneNumber) date

    let _users = deleteUser (tenants hotelCopy) baseForDelete
    let _rooms   = rooms hotelCopy
    let _history = history hotelCopy

    writeIORef hotel (Hotel _users _rooms _history)

    print $ "[DELETE]: New user list -> " ++ show _users
