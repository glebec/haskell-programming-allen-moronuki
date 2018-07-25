module Cipher where

import Data.Char

vigenere, unVigenere :: String -> String -> String
vigenere ""      = id
vigenere keyword = zipWith translate shifts
    where shifts = cycle $ map alphaToNum keyword
unVigenere ""    = id
unVigenere keyword = zipWith translate shifts
    where shifts = cycle $ map (negate . alphaToNum) keyword

-- 65 A to 90 Z, 97 a to 122 z
caesar, unCaesar :: Int -> String -> String
caesar shift = map (translate shift)
unCaesar = caesar . negate

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
