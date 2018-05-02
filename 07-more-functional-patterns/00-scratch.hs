{-# OPTIONS_GHC -W #-}

module ScratchCh7 where

isItTwo :: (Num a, Eq a) => a -> Bool -- why do I have to specify Eq?
isItTwo 2 = True
isItTwo _ = False

-- pattern matching on data constructors

newtype Username = Username String
newtype AccountNumber = AccountNumber Integer

data User = UnregisteredUser
          | RegisteredUser Username AccountNumber

printUser :: User -> IO ()

printUser UnregisteredUser = putStrLn "That user isn't registered."
printUser (
    RegisteredUser (Username name) (AccountNumber num)
    ) = putStrLn $ "User " ++ name ++ " is account " ++ show num

bob = RegisteredUser (Username "Bob") (AccountNumber 1234)

-- sum type:
data WherePenguinsLive =
    Galapagos
  | Antarctica
  | Australia
  | SouthAfrica
  | SouthAmerica deriving (Eq, Show)

-- product type:
data Penguin =
  Peng WherePenguinsLive
  deriving (Eq, Show)

-- Hlint suggests replacing the above with `newtype`.

isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica _ = False

gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng whereitlives) = whereitlives

humboldt = Peng SouthAmerica
gentoo = Peng Antarctica
macaroni = Peng Antarctica
little = Peng Australia
galapagos = Peng Galapagos

-- case expression (HLint suggests just using `if` here):
funcZ x =
  case x + 1 == 1 of
  True -> "AWESOME"
  False -> "wut"
