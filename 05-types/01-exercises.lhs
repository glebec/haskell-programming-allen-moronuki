> module Chapter5Exercises where

\section{5.3}

Type Matching

Match each function to its type signature.

Function | Signature
---------|----------
`not`    | `:: Bool -> Bool`
`length` | `:: [a] -> Int`
`concat` | `:: [[a]] -> [a]`
`head`   | `:: [a] -> a`
`(<)`    | `:: Ord a => a -> a -> Bool`


\section{5.4}

Type Arguments

Given a function and its type, tell us what type results from applying some or all of the arguments.

> f :: a -> a -> a -> a;                   f = undefined
> x :: Char;                               x = undefined
> g :: a -> b -> c -> b;                   g = undefined
> h :: (Num a, Num b) => a -> b -> b;      h = undefined
> jackal :: (Ord a, Eq b) => a -> b -> a;  jackal = undefined
> kessel :: (Ord a, Num b) => a -> b -> a; kessel = undefined

1. `f x :: Char -> Char -> Char`
2. `g 0 'c' "woot" :: Char`
3. `h 1.0 2 :: Num b => b` (I originally said `Integral`… ah, I see.)
4. `h 1 (5.5 :: Double) :: Double`
5. `jackal "keyboard" "has the word jackal in it" :: [Char]`
6. `jackal "keyboard" :: Eq b => b -> [Char]`
7. `kessel 1 2 :: (Num a, Ord a) => a`
8. `kessel 1 (2 :: Integer) :: (Num a, Ord a) => a`
9. `kessel (1 :: Integer) 2 :: Integer`
