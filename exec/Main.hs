{-# LANGUAGE OverloadedStrings, OverloadedLabels, ScopedTypeVariables, LambdaCase, InstanceSigs #-}
{-# OPTIONS_GHC -w #-}

import qualified Data.Text.IO as T
import Data.IORef
import Data.Maybe
import Data.Text (Text, pack, unpack)
import Data.Time.Calendar
import System.Environment (getArgs)

import Control.Monad

import qualified GI.Gtk as Gtk
import Data.GI.Base

import qualified Extractors
import qualified Connectors
import qualified Misc

import HistoryItem
import PersonBase
import HotelCore
import Tenant
import Hotel
import Room

import Combiner

import qualified ViewID as ID

printQuit :: Text -> IO ()
printQuit t = do
    T.putStrLn $ "Quitting by " <> t <> "."
    Gtk.mainQuit
    return ()

onQuit :: Text -> IORef Hotel -> IO ()
onQuit t hotel = do
    hotelCopy <- readIORef hotel
    writeFile "serialized_data.txt" (show hotelCopy)
    printQuit t

main :: IO ()
main = do
    args <- getArgs 
    let targs = map pack args

    Gtk.init $ Just targs
    
    let (filename, dataTxt) = case length args < 2 of 
                                  True -> error "Too few command line arguments."
                                  _ -> (targs!!0, targs!!1)

    print (filename, dataTxt)

    builder <- new Gtk.Builder []
    #addFromFile builder $ filename

    contents :: String <- readFile $ unpack dataTxt
    hotelGlobalInstance <- newIORef $ read contents

    -- let rooms = [Room 1 "Маленькая комната для одиночек" 1000.0 100.50 [] [] [],
    --              Room 2 "Большая раскошная лакшери комната" 3100.0 450.50 [] [] [],
    --              Room 3 "Двух комнтаный номер" 2010.0 200.50 [] [] [],
    --              Room 4 "Семейный номер для семьянинов" 1300.0 150.50 [] [] [],
    --              Room 5 "Лучший номер для уединения" 2100.0 500.50 [] [] [],
    --              Room 6 "Двух этажный номер на верхущке нашего отеля" 10000.0 1000.50 [] [] [],
    --              Room 7 "Кровать в хостеле" 500.0 50.50 [] [] [],
    --              Room 8 "Кровать в хостеле" 500.0 50.50 [] [] [],
    --              Room 9 "Кровать в хостеле" 500.0 50.50 [] [] [],
    --              Room 10 "Кровать в хостеле" 500.0 50.50 [] [] [],
    --              Room 11 "Кровать в хостеле" 500.0 50.0 [] [] [],
    --              Room 12 "Кровать в хостеле" 500.0 50.0 [] [] [],
    --              Room 13 "Маленькая комната для двоих одиночек" 1200.0 150.30 [] [] [],
    --              Room 14 "Двух комнтаный номер" 2010.0 200.29 [] [] [],
    --              Room 15 "Двух комнтаный номер" 2010.0 200.29 [] [] []]

    -- let users = [Tenant (PersonBase "Артемьев" "Чеслав" "89831737682" (fromGregorian 2022 03 08)) "" (-1), 
    --              Tenant (PersonBase "Шевченко" "Юлий" "89028761232"  (fromGregorian 2022 03 08)) "" (-1),
    --              Tenant (PersonBase "Коцюбинский" "Яков" "89811723226"  (fromGregorian 2022 03 08)) "" (-1),
    --              Tenant (PersonBase "Темченко" "Ананий" "89876469004"  (fromGregorian 2022 03 08)) "" (-1),
    --              Tenant (PersonBase "Савин" "Кир" "89054917326"  (fromGregorian 2022 03 08)) "" (-1),
    --              Tenant (PersonBase "Веселов" "Альберт" "89211741199"  (fromGregorian 2022 03 08)) "" (-1),
    --              Tenant (PersonBase "Сысоев" "Харитон" "89841102551"  (fromGregorian 2022 03 08)) "" (-1),
    --              Tenant (PersonBase "Соловьёв" "Цезарь" "89090703370"  (fromGregorian 2022 03 08)) "" (-1),
    --              Tenant (PersonBase "Пестов" "Харитон" "89779534197"  (fromGregorian 2022 03 08)) "" (-1),
    --              Tenant (PersonBase "Терещенко" "Эрик" "89687467322"  (fromGregorian 2022 03 08)) "" (-1),
    --              Tenant (PersonBase "Герасимов" "Никодим" "89197588475"  (fromGregorian 2022 03 08)) "" (-1),
    --              Tenant (PersonBase "Ткаченко" "Харитон" "89931370831"  (fromGregorian 2022 03 08)) "" (-1),
    --              Tenant (PersonBase "Тимошенко" "Цефас" "89828041089"  (fromGregorian 2022 03 08)) "" (-1),
    --              Tenant (PersonBase "Кошелев" "Шерлок" "89801966282"  (fromGregorian 2022 03 08)) "" (-1),
    --              Tenant (PersonBase "Русаков" "Цефас" "89196883334"  (fromGregorian 2022 03 08)) "" (-1),

    --              Tenant (PersonBase "Дубченко" "Биргит" "89630150924"  (fromGregorian 2022 03 08)) "" (-1),
    --              Tenant (PersonBase "Родионова" "Зоя" "89954577800"  (fromGregorian 2022 03 08)) "" (-1),
    --              Tenant (PersonBase "Ковалёва" "Юлия" "89967043807"  (fromGregorian 2022 03 08)) "" (-1),
    --              Tenant (PersonBase "Максимова" "Тамара" "89534705749"  (fromGregorian 2022 03 08)) "" (-1),
    --              Tenant (PersonBase "Рыбакова" "Клавдия" "89583684655"  (fromGregorian 2022 03 08)) "" (-1),
    --              Tenant (PersonBase "Селезнёва" "Жозефина" "89840229024"  (fromGregorian 2022 03 08)) "" (-1),
    --              Tenant (PersonBase "Острожска" "Дина" "89685860144"  (fromGregorian 2022 03 08)) "" (-1),
    --              Tenant (PersonBase "Наумова" "Фаина" "89847696479"  (fromGregorian 2022 03 08)) "" (-1),
    --              Tenant (PersonBase "Куликова" "Инна" "89239909166"  (fromGregorian 2022 03 08)) "" (-1),
    --              Tenant (PersonBase "Константинова" "Октябрина" "89291142022"  (fromGregorian 2022 03 08)) "" (-1),
    --              Tenant (PersonBase "Белозёрова" "Нелли" "89026966321"  (fromGregorian 2022 03 08)) "" (-1),
    --              Tenant (PersonBase "Зимина" "Жаклин" "89683619043"  (fromGregorian 2022 03 08)) "" (-1),
    --              Tenant (PersonBase "Селезнёва" "Клара" "89619067541"  (fromGregorian 2022 03 08)) "" (-1),
    --              Tenant (PersonBase "Кулакова" "Вера" "89882749003"  (fromGregorian 2022 03 08)) "" (-1),
    --              Tenant (PersonBase "Многогрешна" "Вера" "89861859993"  (fromGregorian 2022 03 08)) "" (-1)]

    -- hotelGlobalInstance <- newIORef $ Hotel users rooms []

    Just window <- Misc.getBuilderObj builder "window" Gtk.Window
    on window #destroy $ onQuit "windows close button" hotelGlobalInstance

    Connectors.connectButtonClicked builder ID.create_createUserBtnId $ createUserHandler builder hotelGlobalInstance
    Connectors.connectButtonClicked builder ID.delete_deleteUserBtnId $ deleteUserHandler builder hotelGlobalInstance
    Connectors.connectButtonClicked builder ID.booking_deleteBtnId $ Combiner.deleteBooking builder hotelGlobalInstance

    Connectors.connectComboBoxTextSelect builder ID.room_roomComboBoxID (loadRoomInfo builder hotelGlobalInstance)

    -- Evict from room
    Just evictDialog <- Misc.getBuilderObj builder ID.evict_dialogId Gtk.Dialog

    Connectors.connectButtonClicked builder ID.users_leaveBtnId $ riseEvictDialog builder hotelGlobalInstance
    Connectors.connectButtonClicked builder ID.evict_cancelBtnId $ evictFromDialogCancel evictDialog
    Connectors.connectButtonClicked builder ID.evict_evictBtnId $ evictDialogHandler builder evictDialog hotelGlobalInstance

    -- Rent connectors
    Connectors.connectButtonClicked builder ID.rent_rentBtnId $ rentHandler builder hotelGlobalInstance

    -- Loading 
    loadProfitTable builder hotelGlobalInstance
    loadRooms builder hotelGlobalInstance

    Gtk.main
