> {-# OPTIONS_GHC -W #-}
> {-# LANGUAGE NoMonomorphismRestriction #-}

> module Chapter9Exercises where

\begin{code}
import Data.Bool (bool)
import Data.Char
\end{code}

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


\section{9.8 Spines and nonstrict evaluation}

Bottom Madness: will the following expressions return a value or be ⊥?

\begin{code}
-- b1 =  [x^y | x <- [1..5], y <- [2, undefined]] -- ⊥
b2 =  take 1 $ [x^y | x <- [1..5], y <- [2, undefined]] -- value ([1])
-- b3 =  sum [1, undefined, 3] -- ⊥
b4 =  length [1, 2, undefined] -- value (3)
-- b5 =  length $ [1, 2, 3] ++ undefined -- ⊥
b6 =  take 1 $ filter even [1, 2, 3, undefined] -- value ([2])
-- b7 =  take 1 $ filter even [1, 3, undefined] -- ⊥
b8 =  take 1 $ filter odd [1, 3, undefined] -- value ([1])
b9 =  take 2 $ filter odd [1, 3, undefined] -- value ([1, 3])
-- b10 = take 3 $ filter odd [1, 3, undefined] -- ⊥
\end{code}

For each expression below, determine whether it’s in NF, WHNF, or neither.

1. [1, 2, 3, 4, 5] -- NF
2. 1 : 2 : 3 : 4 : _ -- WHNF
3. enumFromTo 1 10 -- neither
4. length [1, 2, 3, 4, 5] -- neither
5. sum (enumFromTo 1 10) -- neither
6. ['a'..'m'] ++ ['n'..'z'] -- neither
7. (_, 'b') -- WHNF


\section{9.9 Transforming lists of values}

More Bottom

\begin{code}
-- mb1 = take 1 $ map (+1) [undefined, 2, 3] -- ⊥
mb2 = take 1 $ map (+1) [1, undefined, 3] -- value (2)
-- mb3 = take 2 $ map (+1) [1, undefined, 3] -- ⊥

itIsMystery xs = map (\x -> elem x "aeiou") xs -- creates list of bools predicated on vowels

mb5a = map (^2) [1..10] -- [1, 4, 9, 16 etc.]
mb5b = map minimum [[1..10], [10..20], [20..30]] -- [1, 10, 20]
mb5c = map sum [[1..5], [1..5], [1..5]] -- [15, 15, 15]

mb6 = map (\x -> bool x (-x) (x == 3))
\end{code}


\section{9.10 Filtering lists of values}

How might we write a filter function that would give us all the multiples of 3 out of a list from 1-30?

\begin{code}
getMultiple3 = filter (\x -> x `mod` 3 == 0)
howManyMult3 = length . getMultiple3
removeArticles = unwords . filter (\s -> not $ s `elem` ["a", "an", "the"]) . words
\end{code}


\section{9.11 Zipping lists}

\begin{code}
myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x, y) : myZip xs ys

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys

myZipB :: [a] -> [b] -> [(a, b)]
myZipB = zipWith (,)
\end{code}


\section{9.12 Chapter Exercises}

Data.Char:

1. isUpper :: Char -> Bool; toUpper :: Char -> Char

\begin{code}
getUppers :: String -> String
getUppers = filter isUpper

capitalize :: String -> String
capitalize [] = []
capitalize (c:cs) = toUpper c : cs

allCaps :: String -> String
-- allCaps [] = []
-- allCaps (c:cs) = toUpper c : allCaps cs
allCaps = map toUpper

capFirst :: String -> Char
capFirst = toUpper . head
\end{code}

Cipher: see 03-cipher.hs

Implementing standard functions:

\begin{code}

\end{code}
