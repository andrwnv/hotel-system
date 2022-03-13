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

