> {-# OPTIONS_GHC -W #-}

> module Chapter10Exercises where
> import Data.Time
> import Data.Maybe


\section{10.5 Understanding folds}

1. `foldr (*) 1 [1..5]` will return the same as: d) `foldl (*) 1 [1..5]`
2. Write out the evaluation steps for `foldl (flip (*)) 1 [1..3]`:

foldl (flip (*)) 1 [1..3]
foldl (flip (*)) 1 [1, 2, 3]
(((1 *' 1) *' 2) *' 3) -- *' = flip *
((1 *' 2) *' 3)
(2 *' 3)
6

3. One difference between `foldr` and `foldl` is: c) foldr, but not foldl, associates to the right.
4. Folds are catamorphisms, which means they are generally used to: a) reduce structure.
5. Each has at least one error. Fix and test:

> e5a = foldr (++) "" ["woot", "WOOT", "woot"] -- was missing ""
> e5b = foldr max 'a' "fear is the little death" -- corrected from [] to 'a'
> e5c = foldr (&&) True [False, True] -- corrected `and` to `&&`

This one is more subtle than the previous. Can it ever return a different answer?

> e5d = foldr (||) False [False, True] -- corrected `True` to `False`
> -- e5e = foldl (flip ((++) . show)) "" [1..5] -- flipped the reducer
> e5e = foldr ((++) . show) "" [1..5] -- flipped the fold
> e5f = foldr const 0 [1..5] -- changed 'a' to 0
> e5g = foldr const ' ' "tacos" -- changed 0 to ' '
> e5h = foldl (const) 0 "burritos" -- removed the `flip`
> e5i = foldr (flip const) 'z' [1..5] -- changed to `foldr`

(Personally, I think problem 5 is too free-form – too many ways to make the compiler happy without producing a sensible output.)


\section{10.6 Writing folds}

Exercise: Database Processing

> data DatabaseItem = DbString String
>                   | DbNumber Integer
>                   | DbDate   UTCTime
>                   deriving (Eq, Ord, Show)
> theDatabase :: [DatabaseItem]
> theDatabase =
>   [ DbDate (UTCTime
>             (fromGregorian 1911 5 1)
>             (secondsToDiffTime 34123))
>   , DbNumber 9001
>   , DbString "Hello, world!"
>   , DbDate (UTCTime
>             (fromGregorian 1921 5 1)
>             (secondsToDiffTime 34123))
>   ]

1. Write a function that filters for `DbDate` values and returns a list of the `UTCTime` values inside them.

Original version when I first did this chapter:

> -- filterDbDate :: [DatabaseItem] -> [UTCTime]
> -- filterDbDate = map (\(DbDate u) -> u) . filter isDbDate
> --               where isDbDate (DbDate _) = True
> --                     isDbDate _          = False

Re-written much later to use `mapMaybe :: (a -> Maybe b) -> [a] -> [b]`:

> filterDbDate :: [DatabaseItem] -> [UTCTime]
> filterDbDate = mapMaybe getUTC where
>     getUTC (DbDate u) = Just u
>     getUTC _ = Nothing

2. Write a function that filters for `DbNumber` values and returns a list of the Integer values inside them.

> filterDbNumber :: [DatabaseItem] -> [Integer]
> filterDbNumber = map (\(DbNumber n) -> n) . filter isDbNumber
>                where isDbNumber (DbNumber _) = True
>                      isDbNumber _            = False

3. Write a function that gets the most recent date.

> mostRecent :: [DatabaseItem] -> UTCTime
> mostRecent =  maximum . filterDbDate

4. Write a function that sums all of the `DbNumber` values.

> sumDb :: [DatabaseItem] -> Integer
> sumDb = foldr (+) 0 . filterDbNumber

5. Write a function that gets the average of the `DbNumber` values.

> avgDb :: [DatabaseItem] -> Double
> avgDb items = (fromIntegral $ sumDb items) / (fromIntegral . length $ filterDbNumber items)


\section{10.9 Scans}

> fibs :: Integral a => [a]
> fibs = 1 : scanl (+) 1 fibs

> fibsN :: Integral a => a -> a
> fibsN = (!!) fibs . fromIntegral

1. Modify your fibs function to only return the first 20 Fibonacci numbers.

> first20Fibs = take 20 fibs

2. Modify fibs to return the Fibonacci numbers that are less than 100.

> fibsBelow100 = takeWhile (< 100) fibs

3. Try to write the factorial function from Recursion as a scan. You’ll want `scanl` again, and your start value will be `1`. Warning: this will also generate an infinite list, so you may want to pass it through a `take` function or similar.

> factorials :: Integral a => [a]
> factorials = 1 : scanl (*) 1 [2..]


\section{10.10 Chapter Exercises}

Warm-up and review:

> stops  = "pbtdkg"
> vowels = "aeiou"

1a) Write a function that takes inputs from stops and vowels and makes 3-tuples of all possible stop-vowel-stop com- binations.

> aba :: [a] -> [b] -> [(a, b, a)]
> aba as bs = [ (a, b, a') | a <- as, b <- bs, a' <- as ]

> svsWords :: [String]
> svsWords = map toList $ aba stops vowels
>            where toList (a, b, a') = [a, b, a']

1b) Modify that function so that it only returns the combinations that begin with a `p`.

> pWords = filter ((== 'p') . head) svsWords

1c) Now set up lists of nouns and verbs (instead of stops and vowels) and modify the function to make tuples representing possible noun-verb-noun sentences.

> nouns = ["cat", "dog", "hamster", "snake"]
> verbs = ["licks", "eats", "sees", "chases", "sniffs"]
> animalTuples = aba nouns verbs

2) What does the following mystery function do? What is its type?

> seekritFunc x =
>    div (sum (map length (words x)))
>        (length (words x))

Answer: gets average word length of a sentence.

> seekritFunc :: String -> Int

3) We’d really like the answer to be more precise. Can you rewrite that using fractional division?

> meanWordLength :: Fractional a => String -> a
> meanWordLength s =
>    (fromIntegral . sum . map length . words $ s) /
>    (fromIntegral . length . words $ s)

Rewriting functions using folds:

> myOr :: [Bool] -> Bool
> myOr = foldr (||) False

> myAny :: (a -> Bool) -> [a] -> Bool
> myAny f = foldr ((||) . f) False

> myElem :: Eq a => a -> [a] -> Bool
> -- myElem e = foldr ((||) . (== e)) False
> myElem = any . (==)

> myReverse :: [a] -> [a]
> myReverse = foldl (flip (:)) [] -- saw this one already

> myMap :: (a -> b) -> [a] -> [b]
> myMap f = foldr ((:) . f) []

> myFilter :: (a -> Bool) -> [a] -> [a]
> myFilter p = foldr (\el rest -> if p el then el : rest else rest) []

> squish :: [[a]] -> [a]
> -- squish = (>>= id)
> squish = foldr (++) []

> squishMap :: (a -> [b]) -> [a] -> [b]
> -- squishMap = (=<<)
> squishMap f = foldr ((++) . f) []

> squishAgain :: [[a]] -> [a]
> squishAgain = squishMap id

> myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
> myMaximumBy f xs = foldr keepMax (head xs) xs
>                    where keepMax a b = case f a b of
>                                          GT -> a
>                                          _  -> b

> myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
> myMinimumBy f xs = foldr keepMin (head xs) xs
>                    where keepMin a b = case f a b of
>                                          LT -> a
>                                          _  -> b
