{-# LANGUAGE LambdaCase #-}

module WordNumber where

import Data.List (intercalate)

digitToWord :: Int -> String
digitToWord = \case
    1 -> "one"
    2 -> "two"
    3 -> "three"
    4 -> "four"
    5 -> "five"
    6 -> "six"
    7 -> "seven"
    8 -> "eight"
    9 -> "nine"

digits :: Int -> [Int]
-- digits = map ((read :: String -> Int) . (: [])) . show
digits = reverse . stigid where -- reversing so as to avoid recursive `++`
  stigid :: Int -> [Int]
  stigid n
    | n `div` 10 == 0 = [n]
    | otherwise       = n `mod` 10 : stigid (n `div` 10)

wordNumber :: Int -> String
wordNumber = intercalate "-" . map digitToWord . digits
-- replaced `concat . intersperse` with `intercalate` as per hlint

{-
Fill in the implementations of the functions above so that `wordNumber` returns
the English word version of the Int value.
-}
