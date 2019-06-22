{-# LANGUAGE DerivingVia #-}

module Ex26_14.Morra where

import System.Exit (exitSuccess)
import System.Random (Random, random, randomR, randomIO)
import Control.Monad (forever, when)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State

-- 1.
-- Write Morra (Odds and Evens variant)
-- Use StateT and IO
-- state = player & AI scores
-- AI starts play randomly
-- Exit reports scores & congratulates winner

-- 2.
-- human v. human
-- changeover screens

-- 3.
-- AI remembers 3-grams of player behavior and acts accordingly

-- Rules
-- each player enters a number to play (1 | 2)
-- even total = point to p1, odd total = point to p2
-- on exit, declare winner

newtype Score = Score Int deriving (Eq, Show, Num, Ord) via Int

data Game = Game { p1Score :: Score, p2Score :: Score } deriving (Eq, Show)

incP1 :: Game -> Game
incP1 (Game p1 p2) = Game (p1 + 1) p2

incP2 :: Game -> Game
incP2 (Game p1 p2) = Game p1 (p2 + 1)

data Bid = One | Two deriving (Eq, Show, Ord, Enum, Bounded)

-- https://stackoverflow.com/a/11811809/4876305
-- https://stackoverflow.com/a/11816727/4876305
instance Random Bid where
    random gen = randomR (minBound :: Bid, maxBound :: Bid) gen
    randomR (a, b) gen = (toEnum r, g')
        where (r, g') = randomR (fromEnum a, fromEnum b) gen

isEven :: Bid -> Bid -> Bool
isEven One One = True
isEven Two Two = True
isEven _ _ = False

play :: Bid -> Bid -> Game -> Game
play p1 p2 = if isEven p1 p2 then incP1 else incP2

askChar :: String -> IO Char
askChar s = do
    putStr $ s ++ ": "
    getChar <* putChar '\n'

askPlayer :: String -> IO Bid
askPlayer p = do
    bid <- askChar $ p ++ ", please enter your bid (1 or 2)"
    case bid of
        '1' -> pure One
        '2' -> pure Two
        _ -> askPlayer p

askAI :: IO Bid
askAI = randomIO

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J\ESC[H"

getHumanBids :: IO (Bid, Bid)
getHumanBids = do
    b1 <- askPlayer "Player 1"
    clearScreen
    b2 <- askPlayer "Player 2"
    pure (b1, b2)

getHumanAndAIBids :: IO (Bid, Bid)
getHumanAndAIBids = do
    b1 <- askPlayer "Player 1 (Human)"
    b2 <- askAI
    pure (b1, b2)

data Mode = PvP | PvE deriving (Eq, Show)

getMode :: IO Mode
getMode = do
    c <- askChar "Select 1 for PvP, 2 for PvE"
    case c of
        '1' -> pure PvP
        '2' -> pure PvE
        _ -> getMode

endGame :: Score -> Score -> IO ()
endGame s1 s2 = do
    putStrLn $ if s1 > s2 then "Congrats Player 1!" else "Congrats Player 2!"
    exitSuccess

morra :: StateT Game IO a
morra = do
    mode <- lift getMode
    let getBids = if mode == PvP then getHumanBids else getHumanAndAIBids
    forever $ do
        lift clearScreen
        (b1, b2) <- lift getBids
        lift clearScreen
        lift $ putStrLn ("Player 1 played: " ++ show b1)
        lift $ putStrLn ("Player 2 played: " ++ show b2)
        lift $ putStrLn (if isEven b1 b2 then "Player 1 wins" else "Player 2 wins")
        modify $ play b1 b2
        Game s1 s2 <- get
        lift $ putStrLn ("Current scores: " ++ show s1 ++ " " ++ show s2)
        lift $ putStrLn "(Press any key to continue, or ESC to quit)"
        c <- lift $ getChar
        lift $ when (c == '\ESC') (endGame s1 s2)

main :: IO ()
main = evalStateT morra (Game 0 0)
