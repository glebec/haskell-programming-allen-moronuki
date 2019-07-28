module Main where

import Control.Monad (forever, when, unless)
import System.Environment (getArgs)
import System.IO (hPutStr, hGetChar, hWaitForInput, stdout, stdin, isEOF)
import System.Exit (die, exitSuccess)
import Vigenere (Key(..), vigenere, unVigenere)

data Mode = Encrypt | Decrypt deriving (Show)

getKeyAndMode :: IO (Key, Mode)
getKeyAndMode = do
    args <- getArgs
    (key, modeStr) <- case args of
        [] -> die "Please supply a key and mode."
        [_] -> die "Missing mode argument."
        (k:m:_) -> pure (Key k, m)
    mode <- case modeStr of
        "-e" -> pure Encrypt
        "-d" -> pure Decrypt
        _ -> die $ "Unrecognized mode: " ++ modeStr
    pure (key, mode)

main :: IO ()
main = do
    (key, mode) <- getKeyAndMode
    let vig = case mode of
              Encrypt -> vigenere key
              Decrypt -> unVigenere key
    ready <- hWaitForInput stdin 1000
    unless ready $ die "Timeout: no input received"
    forever $ do
        done <- isEOF
        when done exitSuccess
        c <- hGetChar stdin -- HLint suggests `getChar`
        hPutStr stdout $ vig [c] -- HLint suggests `putStr`
