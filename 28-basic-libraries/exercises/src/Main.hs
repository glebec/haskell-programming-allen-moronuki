module Main where

import Benching (benching)
import Ex28_6 (ex28_6)

main :: IO ()
main = do
    benching
    ex28_6
