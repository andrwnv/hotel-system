{-# LANGUAGE OverloadedStrings, OverloadedLabels, ScopedTypeVariables, LambdaCase, InstanceSigs #-}

module HotelCore (collectProfit, replaceRoom, evict, findTenantByBase, replaceUser) where

import Data.Time.Calendar
import Data.Maybe
import GHC.Float

import qualified PersonBase
import qualified Tenant

import HistoryItem
import Hotel
import Room

collectProfit :: [HistoryItem] -> (Double, Double)
collectProfit [] = (0.0, 0.0)
collectProfit (x:xs) = (totalProfit, totalCost)
    where
        currProfitInfo = collectProfit xs
        dates = reservationDate x
        roomCost = dayExpenses (room x)
        daysDiff = fromIntegral $ ((diffDays (dates!!1) (dates!!0)) + 1)
        totalProfit = (fst currProfitInfo) + (totalPrice x)
        totalCost = (snd currProfitInfo) + (daysDiff * roomCost)


_roomsWithoutSelectedRoom :: Int -> [Room] -> [Room]
_roomsWithoutSelectedRoom _ [] = []
_roomsWithoutSelectedRoom roomNum (x:xs)
    | roomNum == roomNumber x = _roomsWithoutSelectedRoom roomNum xs
    | otherwise = [x] ++ _roomsWithoutSelectedRoom roomNum xs

replaceRoom :: Hotel -> Room -> Hotel
replaceRoom hotel newRoom = hotelRes
    where 
        _tenants = tenants hotel
        _rooms = [newRoom] ++ _roomsWithoutSelectedRoom (roomNumber newRoom) (rooms hotel)
        _history = history hotel
        hotelRes = Hotel _tenants _rooms _history

_evict :: Room -> Room
_evict room = updatedRoom
    where
        _roomNumber   = roomNumber room
        _description  = Room.description room
        _tenantPrice  = tenantPrice room
        _dayExpenses  = dayExpenses room
        _plannedRents = plannedRents room
        updatedRoom = Room _roomNumber _description _tenantPrice _dayExpenses [] [] _plannedRents

_calculateTotalPrice :: Room -> Double
_calculateTotalPrice room = totalPrice
    where 
        dates = busyTime room
        price = tenantPrice room
        datesRange = fromIntegral $ ((diffDays (dates!!1) (dates!!0)) + 1)
        totalPrice = price * datesRange

evict :: Hotel -> Room -> String -> Hotel
evict hotel room paymentMethod = hotelRes
    where
        totalPrice = _calculateTotalPrice room
        dates = busyTime room
        newHistoryItem = HistoryItem paymentMethod room totalPrice dates
        _tenants = tenants hotel
        _rooms = [_evict room] ++ _roomsWithoutSelectedRoom (roomNumber room) (rooms hotel)
        _history = [newHistoryItem] ++ history hotel
        hotelRes = Hotel _tenants _rooms _history

_findTenant :: [Tenant.Tenant] -> PersonBase.PersonBase -> Maybe Tenant.Tenant
_findTenant [] _ = Nothing
_findTenant (x:xs) person 
    | isRequiredTenant = Just x
    | otherwise = _findTenant xs person
    where
        xBase = Tenant.base x
        isRequiredTenant = (PersonBase.firstName xBase) == (PersonBase.firstName person) 
                            && (PersonBase.lastName xBase) == (PersonBase.lastName person) 
                            && (PersonBase.phoneNumer xBase) == (PersonBase.phoneNumer person)

findTenantByBase :: Hotel -> PersonBase.PersonBase -> Maybe Tenant.Tenant
findTenantByBase hotel person = _findTenant (tenants hotel) person

_usersWithoutSelectedUser :: Tenant.Tenant -> [Tenant.Tenant] -> [Tenant.Tenant]
_usersWithoutSelectedUser _ [] = []
_usersWithoutSelectedUser tenant (x:xs)
    | tenantBase == xBase = _usersWithoutSelectedUser tenant xs
    | otherwise = [x] ++ _usersWithoutSelectedUser tenant xs
    where
        tenantBase = Tenant.base tenant
        xBase = Tenant.base x

replaceUser :: Hotel -> Tenant.Tenant -> Hotel
replaceUser hotel updatedUser = hotelRes
    where 
        _tenants = [updatedUser] ++ _usersWithoutSelectedUser updatedUser (tenants hotel)
        _rooms = rooms hotel
        _history = history hotel
        hotelRes = Hotel _tenants _rooms _history
