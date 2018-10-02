module Main where

import LearnParsers
import Ex24_3
import Ex24_4
import Ex24_6
import Ex24_7
import Ex24_10
import Ex24_11

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
