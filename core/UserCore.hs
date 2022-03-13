{-# LANGUAGE OverloadedStrings, OverloadedLabels, ScopedTypeVariables, LambdaCase, InstanceSigs #-}

module UserCore where

import PersonBase
import Tenant

type CreateUserSuccess = ([Tenant], Bool)

isValidUser :: PersonBase -> Bool
isValidUser tenant = res
    where
        isValidFirstName = length (firstName tenant) > 2
        isValidLastName = length (lastName tenant) > 2
        isValidPhoneNumber = length (phoneNumer tenant) == 11
        res = isValidFirstName && isValidLastName && isValidPhoneNumber

isUserExist :: [Tenant] -> PersonBase -> Bool
isUserExist [] _ = False
isUserExist (x:xs) potentialUser
    | _base == potentialUser = True
    | otherwise = isUserExist xs potentialUser
    where
        _base = base x

createUser :: [Tenant] -> Tenant -> CreateUserSuccess
createUser tenants newTenant 
    | (not $ isUserExist tenants person) = (res, True)
    | otherwise = (tenants, False)
    where
        person = base newTenant
        res = tenants ++ [newTenant]

isUserEq :: PersonBase -> PersonBase -> Bool
isUserEq p1 p2 = (firstName p1) == (firstName p2) && (lastName p1) == (lastName p2) && (phoneNumer p1) == (phoneNumer p2)

deleteUser :: [Tenant] -> PersonBase -> [Tenant]
deleteUser [] _ = []
deleteUser (x:xs) user
    | isUserEq _base user = deleteUser xs user
    | otherwise = (deleteUser xs user) ++ [x]
    where
        _base = base x
