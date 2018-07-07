module Morse
    ( Morse
    , charToMorse
    , morseToChar
    , stringToMorse
    , letterToMorse
    , morseToLetter
    ) where

import qualified Data.Map as M

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

stringToMorse :: String -> Maybe [Morse]
stringToMorse = sequence . fmap charToMorse

morseToChar :: Morse -> Maybe Char
morseToChar = flip M.lookup morseToLetter
