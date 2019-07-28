module Vigenere (Key(..), vigenere, unVigenere) where

import Data.Char

newtype Key = Key String deriving (Show)

vigenere :: Key -> String -> String
vigenere (Key "") = id
vigenere (Key key) = zipWith translate shifts
    where shifts = cycle $ map alphaToNum key

unVigenere :: Key -> String -> String
unVigenere (Key "") = id
unVigenere (Key key) = zipWith translate shifts
    where shifts = cycle $ map (negate . alphaToNum) key

alphaToNum :: Char -> Int
alphaToNum c
    | isUpper c = code - 65
    | isLower c = code - 71
    | otherwise = code
    where code = ord c

translate :: Int -> Char -> Char
translate shift c
    | isAlpha c = letters !! ((alphaToNum c + shift) `mod` 52)
    | otherwise = c
    where letters = ['A'..'Z'] ++ ['a'..'z']
