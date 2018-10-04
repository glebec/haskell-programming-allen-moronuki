{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Ex24_11_5 where

import Control.Applicative
import Control.Monad (void)
import Text.Trifecta
import Data.Time
import Text.RawString.QQ

-- 5. log file formats: sum time per activity, avg time per activity per day

-- comments: --
-- sections: # 2025-02-05
-- entries:  08:20 Breakfast time

-- make a Show instance for the datatype which matches this format & write a
-- QuickCheck Gen for it so we can test the parser.

data LogEntry = LogEntry TimeOfDay String deriving (Eq, Ord, Show)

data LogSection = LogSection Day [LogEntry] deriving (Eq, Ord, Show)

newtype Log = Log [LogSection] deriving (Eq, Show)

eol :: Parser ()
eol = void (char '\n')

someSpaces :: Parser ()
someSpaces = void $ some (char ' ')

logComment :: Parser ()
logComment = do
    try (string "--")
    manyTill anyChar eol
    pure ()

voidLine :: Parser ()
voidLine = optional someSpaces >> (try eol <|> logComment)

isoDate :: Parser Day
isoDate = do
    y <- read <$> count 4 digit
    char '-'
    m <- read <$> count 2 digit
    char '-'
    d <- read <$> count 2 digit
    pure $ fromGregorian y m d

fullTime :: Parser TimeOfDay
fullTime = do
    h <- read <$> count 2 digit
    char ':'
    m <- read <$> count 2 digit
    pure $ TimeOfDay h m 0

logEntry :: Parser LogEntry
logEntry = do
    t <- fullTime
    someSpaces
    s <- manyTill anyChar (try voidLine)
    pure $ LogEntry t s

logSection :: Parser LogSection
logSection = do
    char '#' >> someSpaces
    d <- isoDate
    voidLine
    es <- some logEntry
    pure $ LogSection d es

parseLog :: Parser Log
parseLog = do
    many voidLine
    ss <- sepBy logSection (many voidLine)
    pure $ Log ss

exLog :: String
exLog = [r|
-- wheee a comment
# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover

# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
|]
