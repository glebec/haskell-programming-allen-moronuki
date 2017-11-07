module Arith3Broken where

main :: IO ()
main = do -- corrected from `Main`
    print (1 + 2) -- corrected from `print 1 + 2`
    print 10
    print (negate (-1)) -- corrected from `negate - 1`
    print ((+) 0 blah)
    where blah = negate 1
