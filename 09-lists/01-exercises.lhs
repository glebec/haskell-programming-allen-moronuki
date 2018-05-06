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
