module Ex23_5 where

import System.Random
import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State

data Die = DieOne
         | DieTwo
         | DieThree
         | DieFour
         | DieFive
         | DieSix
         deriving (Eq, Ord, Show)

-- this should more properly be Int -> Maybe Die
intToDie :: Int -> Die
intToDie n =
    case n of
        1 -> DieOne
        2 -> DieTwo
        3 -> DieThree
        4 -> DieFour
        5 -> DieFive
        6 -> DieSix
        x -> error $ "intToDie got non 1–6 integer: " ++ show x -- avoid this

-- Initial example

rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do
    let s = mkStdGen 2
        (d1, s1) = randomR (1, 6) s
        (d2, s2) = randomR (1, 6) s1
        (d3, _)  = randomR (1, 6) s2
    (intToDie d1, intToDie d2, intToDie d3)

-- with State

rollDie :: State StdGen Die
rollDie = state $ do
    (n, s) <- randomR (1, 6)
    return (intToDie n, s)

rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1, 6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' = liftA3 (,,) rollDie rollDie rollDie

example :: (Die, Die, Die)
example = evalState rollDieThreeTimes' (mkStdGen 0)

infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie

example2 :: [Die]
example2 = evalState infiniteDie (mkStdGen 0)

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

example3 :: [Die]
example3 = evalState (nDie 10) (mkStdGen 0)

-- Exercise Basis

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = go 0 0 g
    where
        go :: Int -> Int -> StdGen -> Int
        go sum count gen
            | sum >= 20 = count
            | otherwise =
                let (die, nextGen) =
                      randomR (1, 6) gen
                in go (sum + die)
                      (count + 1) nextGen

example4 :: Int
example4 = rollsToGetTwenty (mkStdGen 0)

-- Exercise

rollsToGetN :: Int -> StdGen -> (Int, [Die])
rollsToGetN n g = go 0 (0, []) g
    where
        go :: Int -> (Int, [Die]) -> StdGen -> (Int, [Die])
        go sum trace@(count, dieList) gen
            | sum >= n = trace
            | otherwise =
                let (dieNum, nextGen) =
                      randomR (1, 6) gen
                in go (sum + dieNum)
                      (count + 1, dieList ++ [intToDie dieNum]) nextGen

example5 :: (Int, [Die])
example5 = rollsToGetN 34 (mkStdGen 0)
