module Main where

import BadMonoid (checkBadMonoid, checkAppSSI)
import Apl1 (checkList, checkZipList, checkValidation)

main :: IO ()
main = do
    putStrLn "Bad Monoid"
    checkBadMonoid
    putStrLn "Applicative (String, String, Int)"
    checkAppSSI
    putStrLn "List Applicative"
    checkList
    putStrLn "ZipList Applicative"
    checkZipList
    putStrLn "Validation Applicative"
    checkValidation
