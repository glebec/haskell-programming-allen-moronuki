> {-# OPTIONS_GHC -W #-}
> {-# LANGUAGE NoMonomorphismRestriction #-}

> module Chapter9Exercises where

\section{9.5 EnumFromTo}

\begin{code}
eft :: Enum a => a -> a -> [a]
eft a b
  | aIx >  bIx = []
  | aIx == bIx = [a]
  | otherwise  = a : eft (succ a) b
  where aIx = fromEnum a
        bIx = fromEnum b

eftBool = eft :: Bool     -> Bool     -> [Bool]
eftOrd  = eft :: Ordering -> Ordering -> [Ordering]
eftInt  = eft :: Int      -> Int      -> [Int]
eftChar = eft :: Char     -> Char     -> [Char]
\end{code}


\section{9.6}

1. Using `takeWhile` and `dropWhile`, write a function that takes a string and returns a list of strings, using spaces to separate the elements of the string into words.

\begin{code}
myWords :: String -> [String]
myWords = splitOn ' '
-- myWords "" = []
-- myWords s  = takeWhile (/= ' ') s : myWords (dropWhile (== ' ') (dropWhile (/= ' ') s))
\end{code}

2. Next, write a function that takes a string and returns a list of strings, using newline separators to break up the string. (See poemLines.hs)

3. Parameterize.

\begin{code}
splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn c s  = takeWhile (/= c) s : splitOn c (dropWhile (== c) (dropWhile (/= c) s))
\end{code}


\section{9.7 List Comprehensions}

\begin{code}
mySqr = [x^2 | x <- [1..10]] -- [1, 4, 9, 16, 25, 36, 49, 64, 81, 100]

e1 = [x | x <- mySqr, rem x 2 == 0] -- [4, 16, 36, 64, 100]

e2 = [(x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50]
-- [(1, 64), (1, 81), (1, 100), (4, 64) and so on up to (49, 100)]

e3 = take 5 e2 -- [(1, 64), (1, 81), (1, 100), (4, 64), (4, 81)]

mySqrs  = [x^2 | x <- [1..5]]
myCubes = [y^3 | y <- [1..5]]

-- 1. make tuples of the outputs of mySqrs and myCubes.
e1' = [(x, y) | x <- mySqrs, y <- myCubes]

-- 2. alter so that it only uses the input values less than 50
e2' = [(x, y) | x <- mySqrs, y <- myCubes, x < 50, y < 50]

-- 3. determine how many tuples inhabit your output list
e3' = (length e1', length e2') -- 25, 15

\end{code}
