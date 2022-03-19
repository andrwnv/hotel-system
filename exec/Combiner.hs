{-# LANGUAGE OverloadedStrings, OverloadedLabels, ScopedTypeVariables, LambdaCase, InstanceSigs #-}

module Combiner ( createUserHandler, deleteUserHandler
                , loadProfitTable
                , loadRooms, loadRoomInfo ) where

-- Prelude
import Data.Text (Text, unpack, pack)
import Data.IORef
import Data.Maybe
import Control.Monad

-- Gtk
import qualified GI.Gtk as Gtk

-- Core
import HotelCore
import UserCore

-- Entities
import qualified Room as R
import HistoryItem
import PersonBase
import Tenant
import Hotel

-- View
import qualified Misc as MiscView
import qualified Mutation as Mut
import qualified ViewID as ID
import Extractors

-- Utils
import DayChecks


-- User control section
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


-- Profit table section
_loadProfitTable :: Gtk.Builder -> [HistoryItem] -> IO ()
_loadProfitTable _ [] = return ()
_loadProfitTable uiBuilder (x:xs) = do
    Mut.addRowToProfitView uiBuilder ID.profit_tableStoreId profit
    _loadProfitTable uiBuilder xs
    where
        roomNum = show $ R.roomNumber (room x)
        date    = show $ reservationDate x
        _type   = paymentType x
        sum     = totalPrice x
        profit  = MiscView.ProfitView _type roomNum date sum

loadProfitTable :: Gtk.Builder -> IORef Hotel -> IO ()
loadProfitTable uiBuilder hotel = do
    Mut.clearTreeView uiBuilder ID.profit_tableStoreId -- before fill tv., clear all tree view store

    hotelCopy <- readIORef hotel
    _loadProfitTable uiBuilder (history hotelCopy)

    let (profit, cost) = collectProfit (history hotelCopy)
    Mut.changeLabelText uiBuilder ID.profit_sumLabel (pack $ show profit) 
    Mut.changeLabelText uiBuilder ID.profit_costLabel (pack $ show cost) 
    Mut.changeLabelText uiBuilder ID.profit_totalSumLabel (pack $ show (profit - cost)) 

    print $ "[LOADING]: profit view loaded"


-- Booking and rent section
_loadRooms :: Gtk.Builder -> [R.Room] -> IO Bool
_loadRooms _ [] = return False
_loadRooms uiBuilder (x:xs) = do
    let roomNum = (pack $ show (R.roomNumber x))
    Mut.addComboBoxItem uiBuilder ID.room_roomComboBoxID roomNum
    prevStatus <- _loadRooms uiBuilder xs
    let status = True || prevStatus
    return status

loadRooms :: Gtk.Builder -> IORef Hotel -> IO Bool
loadRooms uiBuilder hotel = do
    hotelCopy <- readIORef hotel
    status <- _loadRooms uiBuilder (rooms hotelCopy)

    case status of
        True -> do
            Mut.setSensetiveWidget uiBuilder ID.room_rentLayoutId True
            Mut.setActiveComboBoxItem uiBuilder ID.room_roomComboBoxID 0
            print $ "[LOADING]: Room combo box loaded"
        False -> do 
            Mut.setSensetiveWidget uiBuilder ID.room_rentLayoutId False
            print $ "[LOADING]: Hotel doesnt have rooms"

    return status

_extractSelectedRoom :: [R.Room] -> Int -> Maybe R.Room
_extractSelectedRoom [] _ = Nothing
_extractSelectedRoom (x:xs) num 
    | roomNum == num = Just x
    | otherwise = _extractSelectedRoom xs num
    where
        roomNum = R.roomNumber x

_getTenantsFullNameString :: [Tenant] -> String
_getTenantsFullNameString [] = ""
_getTenantsFullNameString (x:xs) = fullNames
    where
        tenantBase = base x
        fn = PersonBase.firstName tenantBase
        ln = PersonBase.lastName tenantBase
        phone = PersonBase.phoneNumer tenantBase
        fullNames = _getTenantsFullNameString xs ++ fn ++ " " ++ ln ++ " (" ++ phone ++ ")\n"

_fillRoomInfoUI :: Gtk.Builder -> R.Room -> IO ()
_fillRoomInfoUI uiBuilder room = do
    Mut.changeLabelText uiBuilder ID.room_tenantsRentLabel $ pack $ _getTenantsFullNameString (R.busyBy room)
    Mut.changeLabelText uiBuilder ID.room_rentDaysLabel $ pack $ show (R.busyTime room)
    Mut.changeLabelText uiBuilder ID.room_rentPricePerDayLabel $ pack (show(R.tenantPrice room) ++ " руб.") 
    Mut.changeLabelText uiBuilder ID.room_rentDescriptionLabel $ pack (R.description room)
    Mut.changeLabelText uiBuilder ID.room_rentCostLabel $ pack (show(R.dayExpenses room) ++ " руб.") 


_showUserList :: Gtk.Builder -> [Tenant] -> [Tenant] -> IO ()
_showUserList _ [] _ = return ()
_showUserList uiBuilder (x:xs) busyUsers
    | userRoomNumer == (-1) = do 
        Mut.addRowToUserView uiBuilder ID.users_tableStoreId view
        _showUserList uiBuilder xs busyUsers
    | otherwise = _showUserList uiBuilder xs busyUsers
    where 
        xBase = base x
        view = MiscView.UserView (firstName xBase) (lastName xBase) (phoneNumer xBase)
        userRoomNumer = roomNumber x

_fillUsersInRoomInfo :: Gtk.Builder -> [Tenant] -> R.Room -> IO ()
_fillUsersInRoomInfo uiBuilder users room = do
    Mut.clearTreeView uiBuilder ID.users_tableStoreId
    let busyList = R.busyBy room 
    _showUserList uiBuilder users busyList

loadRoomInfo :: Gtk.Builder -> IORef Hotel -> IO ()
loadRoomInfo uiBuilder hotel = do
    hotelCopy <- readIORef hotel
    Just activeRoom <- extractComboBoxText uiBuilder ID.room_roomComboBoxID
    case activeRoom of
        "" -> print "[INFO LOADING]: room info cant be load"
        _  -> do
            let index :: Int = read $ unpack activeRoom
            let room = fromJust $ _extractSelectedRoom (rooms hotelCopy) index
            let users = tenants hotelCopy
            _fillRoomInfoUI uiBuilder room
            _fillUsersInRoomInfo uiBuilder users room
            print $ users
            print $ "[INFO LOADING]: room info loaded " ++ show index
