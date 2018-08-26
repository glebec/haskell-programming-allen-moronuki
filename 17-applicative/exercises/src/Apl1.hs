module Apl1 where

import Control.Applicative -- ZipList
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- 17.8 ZipList

-- orphaned instances

instance Semigroup a => Semigroup (ZipList a) where
    (<>) = liftA2 (<>)

-- -- a wrong monoid, empty ZipLists are the zero instead of the unit
-- instance Monoid a => Monoid (ZipList a) where
--     mempty = ZipList []

-- a working monoid
instance Monoid a => Monoid (ZipList a) where
    mempty = pure mempty
    -- mempty = ZipList $ repeat mempty

-- -- already defined in Test.QuickCheck
-- instance Arbitrary a => Arbitrary (ZipList a) where
--     arbitrary = ZipList <$> arbitrary

instance Eq a => EqProp (ZipList a) where
    (=-=) = eq

-- List Applicative

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Semigroup (List a) where
    Nil <> ys = ys
    Cons x xs <> ys = Cons x $ xs <> ys

instance Applicative List where
    pure x = Cons x Nil
    (<*>) fs xs = flatMap (<$> xs) fs
    -- (<*>) (Cons f fs) xs = (f <$> xs) <> (fs <*> xs)
    -- (<*>) _ _ = Nil

instance Eq a => EqProp (List a) where
    (=-=) = eq

genList :: Arbitrary a => Gen (List a)
genList = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return Nil), (3, return $ Cons a b)]

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = genList

checkList = quickBatch $ applicative (undefined :: List (String, String, Int))

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold (<>) Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap _ Nil = Nil
flatMap f xs = (concat' . fmap f) xs

-- ZipList

take' :: Int -> List a -> List a
take' _ Nil = Nil
take' 0 _   = Nil
take' n (Cons x xs) = Cons x $ take' (n - 1) xs

-- repeat' :: a -> List a
-- repeat' x = let xs = Cons x xs in xs

zipWith' :: (a -> b -> c) -> List a -> List b -> List c
zipWith' _ Nil _ = Nil
zipWith' _ _ Nil = Nil
zipWith' f (Cons a as) (Cons b bs) = Cons (f a b) (zipWith' f as bs)

fromBaseList :: [a] -> List a
fromBaseList = foldr Cons Nil

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
    xs =-= ys = xs' `eq` ys' where
                xs' = let (ZipList' l) = xs in take' 3000 l
                ys' = let (ZipList' l) = ys in take' 3000 l

instance Functor ZipList' where
    fmap f (ZipList' xs) = ZipList' $ f <$> xs

instance Semigroup (ZipList' a) where
    (<>) (ZipList' xs) (ZipList' ys) = ZipList' $ xs <> ys

instance Applicative ZipList' where
    pure = ZipList' . fromBaseList . repeat
    (<*>) (ZipList' Nil) _ = ZipList' Nil
    (<*>) _ (ZipList' Nil) = ZipList' Nil
    (<*>) (ZipList' fs)
          (ZipList' xs) = ZipList' $ zipWith' ($) fs xs

instance Arbitrary a => Arbitrary (ZipList' a) where
    arbitrary = ZipList' <$> arbitrary

checkZipList = quickBatch $
    applicative (undefined :: ZipList' (String, Int, Char))

-- Variations on Either

data Validation e a = FailureV e | SuccessV a deriving (Eq, Show)

instance Functor (Validation e) where
    fmap _ (FailureV e) = FailureV e
    fmap f (SuccessV a) = SuccessV $ f a

instance Monoid e => Applicative (Validation e) where
    pure = SuccessV
    (<*>) (SuccessV f)  (SuccessV x)  = SuccessV $ f x
    (<*>) (FailureV e1) (FailureV e2) = FailureV $ e1 <> e2
    (<*>) (FailureV e)  _             = FailureV e
    (<*>) _             (FailureV e)  = FailureV e

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
    arbitrary = do
        e <- arbitrary
        a <- arbitrary
        frequency [(1, pure $ FailureV e), (3, pure $ SuccessV a)]

instance (Eq e, Eq a) => EqProp (Validation e a) where
    (=-=) = eq

checkValidation = quickBatch $
    applicative (undefined :: Validation String (Int, Int, Int))
