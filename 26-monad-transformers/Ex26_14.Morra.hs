{-# LANGUAGE DerivingVia #-}

module Ex26_14.Morra where

-- import System.Random (randomRIO)
import System.Exit (exitSuccess)
import Control.Monad (forever)
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

newtype Score = Score Int deriving (Eq, Show, Num) via Int

data Game = Game { p1Score :: Score, p2Score :: Score } deriving (Eq, Show)

incP1 :: Game -> Game
incP1 (Game p1 p2) = Game (p1 + 1) p2

incP2 :: Game -> Game
incP2 (Game p1 p2) = Game p1 (p2 + 1)

data Bid = One | Two deriving (Eq, Show)

isEven :: Bid -> Bid -> Bool
isEven One One = True
isEven Two Two = True
isEven _ _ = False

play :: Bid -> Bid -> Game -> Game
play p1 p2 = if isEven p1 p2 then incP1 else incP2

askPlayer :: String -> IO Bid
askPlayer p = do
    putStr $ p ++ ", please enter your bid (1 or 2): "
    bid <- getLine
    case bid of
        [x] | x == '1' -> pure One
            | x == '2' -> pure Two
        _ -> askPlayer p

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J\ESC[H"

morra :: StateT Game IO a
morra = forever $ do
    lift clearScreen
    b1 <- lift $ askPlayer "Player 1"
    lift clearScreen
    b2 <- lift $ askPlayer "Player 2"
    lift clearScreen
    lift $ putStrLn (if isEven b1 b2 then "Player 1 wins" else "Player 2 wins")
    modify $ play b1 b2
    Game s1 s2 <- get
    lift $ putStrLn ("Current scores: " ++ show s1 ++ " " ++ show s2)
    lift getChar

main :: IO ()
main = evalStateT morra (Game 0 0)
