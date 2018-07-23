module Arbitrary where

import Test.QuickCheck

-- Unit types

data Trivial = Trivial deriving (Eq, Show)

trivialGen :: Gen Trivial
trivialGen = return Trivial

instance Arbitrary Trivial where
    arbitrary = trivialGen


-- Tagged polymorphic types

data Identity a = Identity a deriving (Eq, Show)

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = fmap Identity arbitrary
    -- a <- arbitrary
    -- return (Identity a)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = identityGen


-- Product types

data Pair a b = Pair a b deriving (Eq, Show)

pairGen :: (Arbitrary a, Arbitrary b) => Gen (Pair a b)
pairGen = do
    a <- arbitrary
    b <- arbitrary
    return $ Pair a b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
    arbitrary = pairGen

pairGenIntStr :: Gen (Pair Int String)
pairGenIntStr = pairGen


-- Sum types

data Sum a b = First a | Second b deriving (Eq, Show)

sumGen :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGen = do
    a <- arbitrary
    b <- arbitrary
    oneof [return $ First a,
           return $ Second b]

sumGenCharInt :: Gen (Sum Char Int)
sumGenCharInt = sumGen

main' :: IO ()
main' = sample sumGenCharInt
