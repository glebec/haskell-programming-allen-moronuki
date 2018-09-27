module Ex23_7 where

import Control.Monad
import Control.Monad.Trans.State

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5  == 0 = "Buzz"
           | n `mod` 3  == 0 = "Fizz"
           | otherwise       = show n

fizzBuzzList :: [Integer] -> [String]
fizzBuzzList list = execState (mapM_ addResult list) []

addResult :: Integer -> State [String] ()
addResult n = do
    xs <- get
    let result = fizzBuzz n
    put (result : xs)

fizzAndBuzz :: IO ()
fizzAndBuzz =
    mapM_ putStrLn $
        reverse $ fizzBuzzList [1..100]

-- Exercise

fizzBuzzFromTo :: Integer -> Integer -> [String]
fizzBuzzFromTo frm to = execState
    (mapM_ addResult $ enumFromThenTo to (to - 1) frm) []

fizzAndBuzz' :: IO ()
fizzAndBuzz' =
    mapM_ putStrLn $
        fizzBuzzFromTo 1 100
