{-# OPTIONS_GHC -W #-}

module Ch13Exercises where

import Data.Char
import Control.Monad (forever)
import System.Exit (exitSuccess)

-- Hangman: see ./projects/hangman

-- Modifying code

-- 1. Ciphers

vigenere, unVigenere :: String -> String -> String
vigenere keyword = zipWith translate shifts
  where shifts = cycle $ map alphaToNum keyword
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
  where code = ord c

translate :: Int -> Char -> Char
translate shift c
  | isAlpha c = letters !! ((alphaToNum c + shift) `mod` 52)
  | otherwise = c
  where letters = ['A'..'Z'] ++ ['a'..'z']

caesar', vigenere' :: IO ()
caesar' = do
    putStr "Enter plaintext: "
    input <- getLine
    putStr "Enter shift key int: "
    shift <- getLine
    putStrLn $ "Encoded: " ++ caesar (read shift :: Int) input
vigenere' = do
    putStr "Enter plaintext: "
    input <- getLine
    putStr "Enter shift key string: "
    shift <- getLine
    putStrLn $ "Encoded: " ++ vigenere shift input

-- 2.

palindrome :: IO ()
palindrome = forever $ do
    line1 <- getLine
    if line1 == reverse line1
    then putStrLn "It's a palindrome!"
    else do
        putStrLn "Nope!"
        exitSuccess

-- 3.
normalize :: String -> String
normalize = map toLower . filter (\c -> isLetter c && c /= ' ')

palindrome' :: IO ()
palindrome' = forever $ do
    line1 <- getLine
    let line = normalize line1 in
        if line == reverse line
        then putStrLn "It's a palindrome!"
        else do
            putStrLn "Nope!"
            exitSuccess

-- 4.

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid
    = NameEmpty
    | AgeTooLow
    | PersonInvalidUnknown String
    deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
    | name /= "" && age > 0 = Right $ Person name age
    | name == "" = Left NameEmpty
    | age <= 0 = Left AgeTooLow
    | otherwise =
        Left $ PersonInvalidUnknown $
            "Name was: " ++ show name ++
            " Age was: " ++ show age

-- prompt the user for a name and age, try to construct a Person, print result
gimmePerson :: IO ()
gimmePerson = do
    putStr "Enter name: "
    name <- getLine
    putStr "Enter age: "
    age <- (read <$> getLine) :: IO Age
    case mkPerson name age of
        Left reason  -> putStrLn $ "Error: " ++ show reason
        Right person -> putStrLn $ "Yay! " ++ show person
