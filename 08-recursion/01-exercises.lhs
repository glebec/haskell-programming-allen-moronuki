> {-# OPTIONS_GHC -W #-}
> {-# LANGUAGE NoMonomorphismRestriction #-}

> module Chapter8Exercises where

\section{8.2}

> applyTimes 0 f b = b
> applyTimes n f b = f (applyTimes (n-1) f b)

So:

applyTimes 5 (+1) 5
= (+1) (applyTimes 4 (+1) 5)
= (+1) ((+1) (applyTimes 3 (+1) 5))
= (+1) ((+1) ((+1) ((+1) ((+1) applyTimes 0 (+1) 5)))))
= (+1) ((+1) ((+1) ((+1) ((+1) 5)))))
= ((+1) . (+1) . (+1) . (+1) . (+1)) 5
= 10

\section{8.6 Chapter Exercises}

1. What is the type of `[[True, False], [True, True], [False, True]]`? A: `[[Bool]]`

2. Which of the following has the same type as `[[True, False], [True, True], [False, True]]`? A: `[[3 == 3], [6 > 5], [3 < 4]]`

3. For:

> func :: [a] -> [a] -> [a]
> func x y = x ++ y

d) all of the above (x and y are both lists, the same type, and if x is a string y is too.)

4. Which is a valid application of `func` to both of its arguments? A: `func "Hello" "World"`

\section{Reviewing currying}

> cattyConny :: String -> String -> String
> cattyConny x y = x ++ " mrow " ++ y

fill in the types:

> flippy :: String -> String -> String
> flippy = flip cattyConny

> appedCatty :: String -> String
> appedCatty = cattyConny "woops"

> frappe :: String -> String
> frappe = flippy "haha"

1. What is the value of `appedCatty "woohoo!"`? A: "woops mrow woohoo!".
2. `frappe "1"` -> "1 mrow haha"
3. `frappe (appedCatty "2")` -> "woops mrow 2 mrow haha"
4. `appedCatty (frappe "blue")` -> "woops mrow blue mrow haha"
5. `cattyConny (frappe "pink") (cattyConny "green" (appedCatty "blue"))` -> "pink mrow haha mrow green mrow woops mrow blue"
6. `cattyConny (flippy "Pugs" "are") "awesome"` -> "are mrow Pugs mrow awesome"

\section{Recursion}

1. Write out the steps for reducing `dividedBy 15 2` to its final answer according to the Haskell code.

> dividedBy :: Integral a => a -> a -> (a, a)
> dividedBy num denom = go num denom 0
>   where go n d count
>          | n < d     = (count, n)
>          | otherwise = go (n - d) d (count + 1)

`dividedBy 15 2`
`go 15 2 0`
`go 13 2 1`
`go 11 2 2`
`go 9 2 3`
`go 7 2 4`
`go 5 2 5`
`go 3 2 6`
`go 1 2 7`
`(7, 1)`

2. Write a function that recursively sums all numbers from 1 to n, n being the argument. So that if n was 5, you’d add 1 + 2 + 3 + 4 + 5 to get 15. The type should be `(Eq a, Num a) => a -> a`.

> sumtorial :: (Eq a, Num a) => a -> a
> sumtorial n
>   | n == 1    = n
>   | otherwise = n + sumtorial (n - 1)

3. Write a function that multiplies two integral numbers using recursive summation. The type should be `(Integral a) => a -> a -> a`.

> -- only handles positive, not sure how to handle negative without adding (Eq a) or (Ord a).
> multiply :: (Integral a) => a -> a -> a
> multiply factor f2 = go factor f2 0
>   where go 0  _  _ = 0
>         go _  0  _ = 0
>         go 1  f2 r = r + f2 -- unnecessary but more performant…
>         go f1 1  r = r + f1
>         go f1 f2 r = go f1 (f2 - 1) (r + f1)

\section{Fixing dividedBy}

> dividedBy' :: Integral a => a -> a -> Maybe (a, a)
> dividedBy' _   0     = Nothing
> dividedBy' num denom = Just $ go (abs num) (abs denom) 0
>                        where go n d count
>                               | n < d     = (resSign * count, remSign * n)
>                               | otherwise = go (n - d) d (count + 1)
>                              resSign = signum $ num * denom
>                              remSign = signum num

\section{McCarthy 91}

Implement recursively.

> mc91 :: Integral a => a -> a
> mc91 n
>   | n > 100   = n - 10
>   | otherwise = mc91 . mc91 $ n + 11

\section{Numbers into words}

See file.
