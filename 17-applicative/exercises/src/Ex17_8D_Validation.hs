module Ex17_8D_Validation where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

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
