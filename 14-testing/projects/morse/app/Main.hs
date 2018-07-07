module Main where

import Control.Monad (forever, when)
import Data.Traversable (traverse)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (getLine, isEOF)
import Morse (stringToMorse, morseToChar)

main :: IO ()
main = do
    mode <- getArgs
    let argError = do
            putStrLn "Please specify first arg as `from` or `to`"
            exitFailure
    case mode of
        [arg] -> case arg of
            "from" -> convertFromMorse
            "to"   -> convertToMorse
            _      -> argError
        _ -> argError

getLineOrExit :: IO String
getLineOrExit = do
    weAreDone <- isEOF
    when weAreDone exitSuccess
    getLine

convertToMorse :: IO ()
convertToMorse = forever $ do
    line <- getLineOrExit
    let morse = stringToMorse line
    case morse of
        (Just str) -> putStrLn (unwords str)
        Nothing    -> putStrLn ("Error: " ++ line) >> exitFailure

convertFromMorse :: IO ()
convertFromMorse = forever $ do
    line <- getLineOrExit
    let decoded = traverse morseToChar (words line)
    case decoded of
        (Just str) -> putStrLn str
        Nothing    -> putStrLn ("Error: " ++ line) >> exitFailure
