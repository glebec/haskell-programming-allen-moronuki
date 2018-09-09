module Ex20_5 where

import Data.Monoid
import Data.Foldable

-- implement the functions in terms of `foldMap` or `foldr`

-- 1.
sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

-- 2.
product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

-- 3.
elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' e = getAny . foldMap (Any . (== e))

-- 4.
minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldr go Nothing where
    go x Nothing = Just x
    go x (Just y) = Just $ min x y

-- 5.
maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = foldr go Nothing where
    go x Nothing = Just x
    go x (Just y) = Just $ max x y

-- 6.
null' :: Foldable t => t a -> Bool
null' = foldr (\_ _ -> False) True

-- 7.
length' :: Foldable t => t a -> Int
length' = foldr (const (+1)) 0

-- 8.
toList' :: Foldable t => t a -> [a]
toList' = foldr (:) []

-- 9.
fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

-- 10. in terms of foldr
foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\a b -> f a <> b) mempty
