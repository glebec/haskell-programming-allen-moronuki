{-# LANGUAGE FlexibleContexts #-}

module SkiFree where

import Test.QuickCheck
import Test.QuickCheck.Checkers

data S n a = S (n a) a deriving (Eq, Show)

instance Functor n => Functor (S n) where
    fmap f (S na a) = S (f <$> na) (f a)

instance Foldable n => Foldable (S n) where
    foldMap f (S na a) = foldMap f na <> f a

instance ( Functor n
         , Arbitrary (n a)
         , Arbitrary a )
        => Arbitrary (S n a) where
    arbitrary = S <$> arbitrary <*> arbitrary

instance (EqProp a, EqProp (n a)) => EqProp (S n a) where
   (S x y) =-= (S p q) = (x =-= p) .&. (y =-= q)

instance Traversable n => Traversable (S n) where
    sequenceA (S na a) = S <$> sequenceA na <*> a

checkSki = sample' (arbitrary :: Gen (S [] Int))
