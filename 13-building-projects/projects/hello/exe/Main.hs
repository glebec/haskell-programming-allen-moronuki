module Main where

import Hello
import DogsRule
import System.IO

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStr "Please enter your name: "
    name <- getLine
    sayHello name
    dogs

-- main = putStrLn "Please enter your name:"
--        >> getLine
--        >>= \name -> sayHello name
--        >> dogs
