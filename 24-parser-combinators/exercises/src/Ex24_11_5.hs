{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Ex24_11_5 where

import Control.Applicative
import Control.Monad (void)
import Data.List
import Data.Foldable (concatMap)
import Text.Trifecta
import Data.Time
import Text.RawString.QQ
import qualified Data.Map as Map

-- 5. log file formats: sum time per activity, avg time per activity per day

-- comments: --
-- sections: # 2025-02-05
-- entries:  08:20 Breakfast time

-- make a Show instance for the datatype which matches this format & write a
-- QuickCheck Gen for it so we can test the parser.

data LogEntry = LogEntry
    { getTime :: TimeOfDay
    , getEntry :: String }
    deriving (Eq, Ord)

data LogSection = LogSection
    { getDay :: Day
    , getEntries :: [LogEntry] }
    deriving (Eq, Ord)

newtype Log = Log
    { getSections :: [LogSection] }
    deriving (Eq)

instance Show Log where
    show (Log ss) = intercalate "\n\n" $ map show ss

instance Show LogSection where
    show (LogSection d es) =
        showGregorian d ++ "\n" ++
        intercalate "\n" (map show es)

instance Show LogEntry where
    show (LogEntry t s) = formatTime defaultTimeLocale "%R" t ++ " " ++ s

-- data processing

-- Sum the time spent in each activity.

toDatedEntries :: Log -> [(LocalTime, String)]
toDatedEntries (Log ss) = concatMap convert ss where
    convert (LogSection d es) = fmap (toLocal d) es
    toLocal d e = (LocalTime d $ getTime e, getEntry e)

timePerActivity :: Log -> [(String, NominalDiffTime)]
timePerActivity l = Map.toAscList $ foldr go Map.empty es' where
    es = sort $ toDatedEntries l
    es' = zip es (tail es)
    go ((t, e), (t', _)) m = let d = diffLocalTime t' t in
        if Map.member e m
        then Map.adjust (+ d) e m
        else Map.insert e d m

-- Provide an alternative aggregation of the data that provides average time
-- spent per activity per day.

meanTimePerActivityPerDay :: Log -> [(String, NominalDiffTime)]
meanTimePerActivityPerDay l@(Log ss) =
    (fmap . fmap) (/ fromIntegral (length ss)) (timePerActivity l)

-- parser

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

# 2025-02-07   -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up...    headache   --   whitespace
13:37 Go to medbay
|]
