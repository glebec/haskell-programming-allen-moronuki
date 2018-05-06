module PoemLines where

-- Turns out Haskell module names and file names must match. Blast.
-- import Chapter9Exercises (splitOn)
splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn c s  = takeWhile (/= c) s : splitOn c (dropWhile (== c) (dropWhile (/= c) s))

firstSen  = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen  = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful\
           \ symmetry?" -- example of multiline strings

sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

-- `putStrLn sentences` should print:
-- Tyger Tyger, burning bright
-- In the forests of the night
-- What immortal hand or eye
-- Could frame thy fearful symmetry?

-- Implement this
myLines :: String -> [String]
-- myLines "" = []
-- myLines s  = takeWhile (/= '\n') s : myLines (dropWhile (== '\n') (dropWhile (/= '\n') s))
myLines = splitOn '\n'

-- What we want 'myLines sentences' to equal
shouldEqual =
    [ "Tyger Tyger, burning bright"
    , "In the forests of the night"
    , "What immortal hand or eye"
    , "Could frame thy fearful symmetry?"
    ]

-- The main function here is a small test to ensure
-- you've written your function correctly.
main :: IO ()
main = print $
    "Are they equal? " ++ show (myLines sentences == shouldEqual)
