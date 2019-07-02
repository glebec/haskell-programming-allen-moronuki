module Main where

import Benching (benching)
import Seqs (benchSeqs)
import Ex28_6 (ex28_6)

main :: IO ()
main = do
    -- benching
    -- ex28_6
    benchSeqs
