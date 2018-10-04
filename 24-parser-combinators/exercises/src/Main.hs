module Main where

import LearnParsers
import Ex24_3
import Ex24_4
import Ex24_6
import Ex24_7
import Ex24_10
import Ex24_11_1
import Ex24_11_2
import Ex24_11_3
import Ex24_11_4
import Ex24_11_5
import Ex24_11_6to9
import Ex24_11_10

main :: IO ()
main = do
    demoParsers
    demo24_3
    demoFraction
    demoIntEof
    demo24_6
    testParseIni
    demo24_10
    checkSemVer
