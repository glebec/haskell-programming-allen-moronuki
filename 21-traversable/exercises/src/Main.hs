module Main where

import Data.Proxy (Proxy(..))
import Test.QuickCheck
import Classes
import Test.QuickCheck.Checkers
import CheckersClasses
import Ex21_8_Wreq (printRespBodies)
import Ex21_12
import SkiFree
import Tree

header :: String -> IO ()
header s = putStrLn $ "\n\n" ++ s

main :: IO ()
main = do
    printRespBodies
    header "traversable demo: []"
    lawsCheck $ traversableLaws (Proxy :: Proxy [])
    header "traversable Identity"
    checkTrIdentity
    header "traversable S"
    -- checkSki
    let trigger :: S [] (Int, Int, [Int])
        trigger = undefined
    quickBatch (traversable trigger)
    header "traversable Tree"
    quickBatch $ traversable (undefined :: Tree (Int, Int, [Int]))
