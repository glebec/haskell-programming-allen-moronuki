module Ex21_6_Morse where

import qualified Data.Map as M
import Data.Maybe

-- new code from 21-traversable

stringToMorse :: String -> Maybe [Morse]
stringToMorse = traverse charToMorse -- changed from `sequence . fmap`

morse :: String -> [Morse]
morse = fromMaybe [] . stringToMorse

x :: Maybe String
x = traverse morseToChar (morse "gabriel")

-- old code from 14-testing

type Morse = String

letterToMorse :: (M.Map Char Morse)
letterToMorse = M.fromList
    [ ('a', ".-")
    , ('b', "-...")
    , ('c', "-.-.")
    , ('d', "-..")
    , ('e', ".")
    , ('f', "..-.")
    , ('g', "--.")
    , ('h', "....")
    , ('i', "..")
    , ('j', ".---")
    , ('k', "-.-")
    , ('l', ".-..")
    , ('m', "--")
    , ('n', "-.")
    , ('o', "---")
    , ('p', ".--.")
    , ('q', "--.-")
    , ('r', ".-.")
    , ('s', "...")
    , ('t', "-")
    , ('u', "..-")
    , ('v', "...-")
    , ('w', ".--")
    , ('x', "-..-")
    , ('y', "-.--")
    , ('z', "--..")
    , ('0', "-----")
    , ('1', ".----")
    , ('2', "..---")
    , ('3', "...--")
    , ('4', "....-")
    , ('5', ".....")
    , ('6', "-....")
    , ('7', "--...")
    , ('8', "---..")
    , ('9', "----.")
    ]

morseToLetter :: M.Map Morse Char
morseToLetter = M.foldrWithKey (flip M.insert) M.empty letterToMorse

charToMorse :: Char -> Maybe Morse
charToMorse = flip M.lookup letterToMorse

morseToChar :: Morse -> Maybe Char
morseToChar = flip M.lookup morseToLetter

