module Main where

import BadMonoid (checkBadMonoid, checkAppSSI)
import Ex17_8A_ZipList
import Ex17_8B_List (checkList)
import Ex17_8C_ZipList (checkZipList)
import Ex17_8D_Validation (checkValidation)
import Ex17_9A ( checkPair
               , checkTwo
               , checkThree
               , checkThree'
               , checkFour
               , checkFour' )

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
    putStrLn "Pair, Two, Three, Three', Four, Four'"
    checkPair
    checkTwo
    checkThree
    checkThree'
    checkFour
    checkFour'
