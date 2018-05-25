module Cipher where

import Data.Char

-- 65 A to 90 Z, 97 a to 122 z
caesar, unCaesar :: Int -> String -> String
caesar shift = map (translate shift)
unCaesar = caesar . negate

toAlphaNum :: Char -> Int
toAlphaNum c
  | isUpper c = code - 65
  | isLower c = code - 71
  where code = ord c

translate :: Int -> Char -> Char
translate shift c
  | isAlpha c = letters !! ((toAlphaNum c + shift) `mod` 52)
  | otherwise = c
  where letters = ['A'..'Z'] ++ ['a'..'z']
