> {-# OPTIONS_GHC -W #-}
> {-# LANGUAGE NoMonomorphismRestriction #-}

> module Chapter7Exercises where

\section{7.3 Anonymous Functions}

1. Which (two or more) of the following are equivalent?

a) mTh x y z = x * y * z
b) mTh x y = \z -> x * y * z
c) mTh x = \y -> \z -> x * y * z
d) mTh = \x -> \y -> \z -> x * y * z

A: all of them.

> mTh = \x y z -> x * y * z

2. Which is the type of `mTh 3`?

> -- mTh 3 :: Num a => a -> a -> a

3.

a) Rewrite the f function in the where clause.

> addOneIfOdd n = case odd n of
>     True -> f n
>     False -> n
>     where f n = n + 1

> addOneIfOdd' n = case odd n of
>     True -> f n
>     False -> n
>     where f = \n -> n + 1

b) Rewrite the following to use anonymous lambda syntax:

> addFive x y = (if x > y then y else x) + 5
> addFive' = \x y -> (if x > y then y else x) + 5

c) Rewrite the following so that it doesn’t use anonymous lambda syntax:

> mflip f = \x -> \y -> f y x
> mflip' f x y = f y x


\section{7.4 Pattern Matching}

1. Given the following declarations:

> k (x, y) = x
> k1 = k ((4 - 1), 10)
> k2 = k ("three", (1 + 2))
> k3 = k (3, True)

a) What is the type of k?

> k :: (a, b) -> a

b) What is the type of k2? Is it the same type as k1 or k3?

> k2 :: String -- not the same as k1 or k3

c) Of k1, k2, k3, which will return the number 3 as the result?

> res1c = k1 == 3 && k3 == 3 -- True

2. Fill in the definition of the following function. Remember: Tuples have the same syntax for their type constructors and their data constructors.

> f :: (a, b, c)
>   -> (d, e, f)
>   -> ((a, d), (c, f))
> f (a, _, c) (d, _, f) = ((a, d), (c, f))


\section{7.5 Case Expressions}

Rewrite if-then-else expressions into case expressions.

> functionC x y = if (x > y) then x else y
> functionC' x y = case x > y of
>   True -> x
>   False -> y

> ifEvenAdd2 n = if even n then (n+2) else n
> ifEvenAdd2' n = case even n of
>   True -> n + 2
>   False -> n

> nums x =
>   case compare x 0 of
>     LT -> -1
>     GT -> 1
>     EQ -> 0


\section{7.6 Higher-Order Functions}

Given the following definitions tell us what value results from further applications.

> dodgy x y = x + y * 10
> oneIsOne = dodgy 1
> oneIsTwo = (flip dodgy) 2

1. `dodgy 1 0` 1
2. `dodgy 1 1` 11
3. `dodgy 2 2` 22
4. `dodgy 1 2` 21
5. `dodgy 2 1` 12
6. `oneIsOne 1` 11
7. `oneIsOne 2` 21
8. `oneIsTwo 1` 21
9. `oneIsTwo 2` 22
10. `oneIsOne 3` 31
11. `oneIsTwo 3` 23


\section{7.7 Guards}

1. With `otherwise` at the top, we always get that case.
2. Cases evaluate from top to bottom, so we get a false answer 'C'.

> pal xs
>   | xs == reverse xs = True
>   | otherwise        = False

3. b) `True` when `xs` is a palindrome
4. `Eq a => [a]`
5. `Eq a => [a] -> Bool`

> numbers x
>   | x < 0  = -1
>   | x == 0 = 0
>   | x > 0  = 1

6. c) an indication of whether its argument is a positive or negative number or zero
7. `(Num a, Ord a) => a`
8. `(Ord a, Num a, Num p) => a -> p`


\section{7.11 Chapter Exercises}

Multiple Choice:

1. d) may resolve to values of different types, depending on inputs

(Though this can be true, it is not always true, i.e. it is not a definition of polymorphism as I understand it. Perhaps I am misunderstanding something.)

2. Two functions named `f` and `g` have types `Char -> String` and `String -> [String]` respectively. The composed function `g . f` has the type `Char -> [String]`.

3. A function `f` has the type `Ord a => a -> a -> Bool` and we apply it to one numeric value. What is the type now? A: `Ord a => a -> Bool`.

4. A function with the type `(a -> b) -> c`… is a higher-order function.

5. what is the type of `f' True`? A: `Bool`

> f' :: a -> a
> f' x = x

Writing Code:

1. The following function returns the tens digit of an integral argument.

> tensDigit :: Integral a => a -> a
> tensDigit x = d
>   where xLast = x `div` 10
>         d     = xLast `mod` 10

a) with `divMod`:

> tensDigit' x = snd $ x `divMod` 10

b) same type
c) hundreds digit version:

> hundredsDigit :: Integral a => a -> a
> -- hundredsDigit x = x `div` 100 `mod` 10
> hundredsDigit = (`mod` 10) . (`div` 100)

2. Implement the function of the type `a -> a -> Bool -> a` once each using a case expression and once with a guard.

> foldBool :: a -> a -> Bool -> a
> foldBool a b p
>   | p == True  = a
>   | p == False = b

> foldBool' :: a -> a -> Bool -> a
> foldBool' a b p = case p of
>   True  -> a
>   False -> b

3. Fill in the definition. Note that the first argument to our function is also a function which can be applied to values. Your second argument is a tuple, which can be used for pattern matching:

> g :: (a -> b) -> (a, c) -> (b, c)
> g f (a, c) = (f a, c)

4 & 5: `roundTrip = read . show`

6. Using `(Show a, Read b) => a -> b` for the Arith4 module, make `print (roundTrip 4)` work.

(I assume that `print (roundTrip 4 :: Int)` is fair game here?)
