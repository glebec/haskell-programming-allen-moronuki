module Scratch12 where

doAliasesProvideSafety :: String -> Int -> Bool
doAliasesProvideSafety n a = False -- accepts `name` and `age`

result = doAliasesProvideSafety ("X" :: Name) (99 :: Age) -- False

-----------------------

type Name = String
type Age  = Int
type ValidatePerson a = Either [PersonInvalid] a

data PersonInvalid = NameEmpty
                   | AgeTooLow
                   deriving (Show, Eq)

data Person = Person Name Age deriving (Eq, Show)

mkPerson :: Name -> Age -> ValidatePerson Person
mkPerson name age = mkPerson' (nameOK name) (ageOK age)

mkPerson' :: ValidatePerson Name
          -> ValidatePerson Age
          -> ValidatePerson Person
mkPerson' (Right name)    (Right age)     = Right $ Person name age
mkPerson' (Left nReasons) (Left aReasons) = Left (nReasons ++ aReasons)
mkPerson' (Left nReasons) _               = Left nReasons
mkPerson' _               (Left aReasons) = Left aReasons

nameOK :: Name -> Either [PersonInvalid] Name
nameOK name = case name of
    "" -> Left [NameEmpty]
    _  -> Right name

ageOK :: Age -> Either [PersonInvalid] Age
ageOK age
  | age <= 0  = Left [AgeTooLow]
  | otherwise = Right age
