module Ex22_02 where

import Data.Char

cap :: String -> String
cap = map toUpper

rev :: String -> String
rev = reverse

composed :: String -> String
composed = cap . rev

fmapped :: String -> String
fmapped = fmap cap rev

tupled :: String -> (String, String)
tupled = (,) <$> composed <*> fmapped

tupledM :: String -> (String, String)
tupledM = do
    a <- composed
    b <- fmapped
    return (a, b)

tupledM' :: String -> (String, String)
tupledM' = composed >>= \a ->
           fmapped >>= \b ->
           const (a, b)
