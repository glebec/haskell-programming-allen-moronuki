module ApplicativesInUse where

import Data.List (elemIndex)
import Data.Monoid

-- Exercises: Lookups (correct using `pure`, `<$>`, and `<*>`)

-- 1. added = (+3) (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

added :: Maybe Integer
added = (+3) <$> lookup 3 (zip [1, 2, 3] [4, 5, 6])

-- 2. tupled = (,) y z

y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

-- 3. maxed = max' x y

xi :: Maybe Int
xi = elemIndex 3 [1, 2, 3, 4, 5]

yi :: Maybe Int
yi = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> xi <*> yi

-- 4. summed = sum $ (,) x y

xs = [1, 2, 3]
ys = [4, 5, 6]

x' :: Maybe Integer
x' = lookup 3 $ zip xs ys

y' :: Maybe Integer
y' = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = fmap sum $ (,) <$> x' <*> y'

-- weird, never used `pure`. Oh well.

-- Exercise: Applicative Instance for Identity

newtype Identity a = Identity a
    deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
    pure  = Identity
    (<*>) (Identity f) (Identity a) = Identity (f a)

-- Exercise: Applicative Instance for Constant

newtype Constant a b = Constant { getConstant :: a }
    deriving (Eq, Ord, Show)

instance Functor (Constant a) where
    fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
    pure a = Constant mempty
    (<*>) (Constant a) (Constant a') = Constant $ a <> a'

-- Exercise: Fixer Upper

-- Use `<$>`, `<*>`, and `pure` to make the code work

-- 1. const <$> Just "Hello" <*> "World"

fu1 = const <$> Just "hello" <*> pure "World"

-- 2. (,,,) Just 90 <*> Just 10 Just "Tierness" [1, 2, 3]


fu2 = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]
