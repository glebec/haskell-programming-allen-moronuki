module Ex17_8C_ZipList where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Ex17_8B_List

-- ZipList

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
