{-# OPTIONS_GHC -W #-}

module Ch12Exercises where

import Data.Maybe (fromMaybe, fromJust)
-- import Data.Monoid ((<>))

-- Determine the kinds

-- 1. The kind of `a` in `id :: a -> a` is `*`.
-- 2. The kinds of `a` and `f` in `r :: a -> f a` are `*` and `* -> *` respectively.

-- String processing

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe str   = Just str

-- 1. (use recursion)
replaceThe :: String -> String
-- I find this example a bit contrived. I would just use `map`.
-- replaceThe = unwords . map (fromMaybe "a" . notThe) . words
replaceThe = unwords . go . map notThe . words
  where go :: [Maybe String] -> [String]
        go [] = []
        go (w : ws) = fromMaybe "a" w : go ws

-- 2.
-- Throwing `Maybe` types into this example problem seems
-- like a poor fit. It's much simpler without it.

startsWithVowel :: String -> Bool
startsWithVowel ""     = False
startsWithVowel (c:_) = c `elem` vowels

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = go . words
  where go :: [String] -> Integer
        go []  = 0
        go [_] = 0
        go (w : x : ws) = case (w, startsWithVowel x) of
            ("the", True) -> 1 + go ws
            _             -> go (x : ws)

-- 3.
countVowels :: String -> Integer
-- countVowels = foldr plusIfVowel 0
--   where plusIfVowel c n = if c `elem` vowels
--                           then n + 1
--                           else n
countVowels = fromIntegral . length . filter (`elem` ("AEIOUaeiou" :: String))

-- Validate the word

newtype Word' = Word' String deriving (Eq, Show)

vowels = "AEIOUaeiou" :: String
isVowel = (`elem` vowels)

mkWord :: String -> Maybe Word'
mkWord s
  | numVowels > numConsonants = Nothing
  | otherwise = Just (Word' s)
  where numVowels = countVowels s
        numConsonants = fromIntegral (length s) - numVowels

-- It's only Natural

data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ a) = 1 + natToInteger a

integerToNat :: Integer -> Maybe Nat
integerToNat i
  | i < 0     = Nothing
  | otherwise = Just $ convert i
  where convert 0 = Zero
        convert a = Succ $ convert (a - 1)

-- Small library for Maybe

-- 1.
isJust :: Maybe a -> Bool
isJust Nothing  = False
isJust (Just _) = True

isNothing :: Maybe a -> Bool
isNothing = not . isJust

-- 2.
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee fallback _ Nothing = fallback
mayybee _ mapper (Just el) = mapper el
-- mayybee fallback mapper m = case m of
--     Nothing  -> fallback
--     (Just v) -> mapper v

-- 3.
fromMaybe' :: a -> Maybe a -> a
-- fromMaybe' val Nothing  = val
-- fromMaybe' _ (Just val) = val
fromMaybe' fallback = mayybee fallback id

-- 4.
listToMaybe :: [a] -> Maybe a
listToMaybe []    = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just x) = [x]

-- 5.
catMaybes :: [Maybe a] -> [a]
-- catMaybes = foldr takeJust []
--   where takeJust Nothing  xs = xs
--         takeJust (Just x) xs = x : xs
catMaybes = map fromJust . filter isJust

-- 6.
flipMaybe :: [Maybe a] -> Maybe [a]
-- flipMaybe list
--   | length result == length list = Just result
--   | otherwise                    = Nothing
--   where result = go list
--         go []     = []
--         go (x:xs) = maybe [] (: go xs) x
flipMaybe list = if any isNothing list
                 then Nothing
                 else Just $ catMaybes list

-- Small library for Either

-- 1.
lefts' :: [Either a b] -> [a]
lefts' = foldr takeLeft []
  where takeLeft (Right _) xs = xs
        takeLeft (Left x)  xs = x:xs

-- Prelude reveals something I didn't know / remember:
-- List comprehensions can use pattern matching!
-- lefts xs = [x | Left x <- xs]

-- Addendum: and since [] is the fail case for list monad, we can do:
-- lefts xs = do { Left x <- xs; pure x }

-- 2.
rights' :: [Either a b] -> [b]
rights' = foldr takeRight []
  where takeRight (Right x) xs = x:xs
        takeRight (Left _)  xs = xs

-- 3.
partitionEithers' :: [Either a b] -> ([a], [b])
-- my initial implementation using mappend:
-- partitionEithers' [] = ([], [])
-- partitionEithers' (e:es) = case e of
--     Left  a -> ([a], []) <> partitionEithers' es
--     Right b -> ([], [b]) <> partitionEithers' es

-- a revised version which avoids implicit ++:
partitionEithers' [] = ([], [])
partitionEithers' (e:es) = case e of
    Left  a -> ((a:), id) <$$> partitionEithers' es
    Right b -> (id, (b:)) <$$> partitionEithers' es
    where (<$$>) (f, g) (a, b) = (f a, g b)

-- a much simpler implementation (LOL):
-- partitionEithers' es = (lefts' es, rights' es)

-- Prelude simply does a foldr:
-- partitionEithers = foldr (either left right) ([],[])
--  where
--   left  a ~(l, r) = (a:l, r)
--   right a ~(l, r) = (l, a:r)

-- 4.
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f e = case e of
    Left  _ -> Nothing
    Right b -> Just $ f b

-- 5.
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a)  = f a
either' _ g (Right b) = g b

-- 6.
eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)

-- Unfolds

-- Write your own iterate and unfoldr

-- 1.
myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

-- 2.
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr gen seed = case gen seed of
    Just (el, newSeed) -> el : myUnfoldr gen newSeed
    Nothing -> []

-- 3.
betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\o -> Just (o, f o))

-- Tree

data BinaryTree a = Leaf
                  | Node (BinaryTree a) a (BinaryTree a)
                  deriving (Eq, Ord, Show)

-- 1.
unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold gen seed = case gen seed of
    Just (l, x, r) -> Node (unfold gen l) x (unfold gen r)
    Nothing -> Leaf

-- 2.
treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold gen 0
    where gen x = if x == n then Nothing else Just (x + 1, x, x + 1)
