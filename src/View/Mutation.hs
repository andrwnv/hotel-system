{-# LANGUAGE OverloadedStrings, OverloadedLabels, ScopedTypeVariables, LambdaCase, InstanceSigs #-}

module View.Mutation where

data ProfitView = ProfitView {
    profitType   :: String,
    roomNumber   :: String,
    date         :: String,
    sum          :: Double
} deriving (Show)

data VacationView = VacationView {
    fullName    :: String,
    startDay    :: String,
    endDay      :: String
} deriving (Show)



