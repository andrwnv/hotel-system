{-# LANGUAGE ScopedTypeVariables #-}

module ViewID where

import Data.Text (Text, pack)

-- Create user id's
create_LastNameFieldID      :: Text = pack "createLastNameField"
create_FirstNameFieldID     :: Text = pack "createFirstNameField"
create_PhoneNumberFieldID   :: Text = pack "createPhoneNumberField"
create_birthDayCalendarID   :: Text = pack "birthDayCal"
create_EmailFieldID         :: Text = pack "createEmailField"
create_createUserBtnId      :: Text = pack "createUser"

-- Delete user id's
delete_LastNameFieldID      :: Text = pack "deleteLastNameField"
delete_FirstNameFieldID     :: Text = pack "deleteFirstNameField"
delete_PhoneNumberFieldID   :: Text = pack "deletePhoneNumberField"
delete_deleteUserBtnId      :: Text = pack "deleteUser"

-- Profit id's
profit_tableViewId          :: Text = pack "profitTree"
profit_tableStoreId         :: Text = pack "profitStore"
profit_sumLabel             :: Text = pack "profitLabel"
profit_costLabel            :: Text = pack "costLabel"
profit_totalSumLabel        :: Text = pack "totalProfitLabel"

-- Room id's
room_roomComboBoxID         :: Text = pack "roomComboBox"
room_rentLayoutId           :: Text = pack "rentLayout"
room_tenantsRentLabel       :: Text = pack "tenantsRentLabel"
room_rentDaysLabel          :: Text = pack "rentDaysLabel"
room_rentPricePerDayLabel   :: Text = pack "rentPricePerDayLabel"
room_rentDescriptionLabel   :: Text = pack "rentDescriptionLabel"
room_rentCostLabel          :: Text = pack "rentCostLabel"

-- User id's
users_tableStoreId          :: Text = pack "userStore"

-- Booking id's
booking_tableStoreId        :: Text = pack "bookingStore"

