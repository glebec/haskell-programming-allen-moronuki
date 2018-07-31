{-# LANGUAGE TypeApplications #-}

module Main where

import Test.Hspec
import Test.QuickCheck
-- import Control.Monad
-- import Data.Monoid
-- import MadLib
import Data.List.NonEmpty

-- Exercise: Optional Monoid

data Optional a = Nada
                | Only a
                deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
    (<>) Nada     x         = x
    (<>) x        Nada      = x
    (<>) (Only a) (Only a') = Only (a <> a')

instance Monoid a => Monoid (Optional a) where
    mempty = Nada

-- genOptional :: Arbitrary a => Gen (Optional a)
-- genOptional = arbitrary >>= \a -> elements [Nada, Only a]

-- genOptional3 :: Arbitrary a => Gen (Optional a, Optional a, Optional a)
-- genOptional3 = (,,) <$> genOptional <*> genOptional <*> genOptional

-- uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
-- uncurry3 f (a, b, c) = f a b c

-- prop_assocOptional :: Property
-- prop_assocOptional = forAll (genOptional3 :: Gen (Optional String, Optional String, Optional String))
--     $ uncurry3 prop_associative

prop_associative :: (Semigroup a, Eq a) => a -> a -> a -> Bool
prop_associative a b c = (a <> b) <> c == a <> (b <> c)

-- I felt that my QC code above was overwrought. Li-yao Xia suggested this:
instance Arbitrary a => Arbitrary (Optional a) where
    arbitrary = oneof [pure Nada, Only <$> arbitrary]

-- Testing Asc

asc :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
asc (<>) a b c =
    a <> (b <> c) == (a <> b) <> c

-- Monoid Identity

prop_monoidLeftId :: (Monoid a, Eq a) => a -> Bool
prop_monoidLeftId a = mempty <> a == a

prop_monoidRightId :: (Monoid a, Eq a) => a -> Bool
prop_monoidRightId a = a == a <> mempty

-- Bad Monoid

data Bull = Fools
          | Twoo
          deriving (Eq, Show)

instance Arbitrary Bull where
    arbitrary = frequency [ (1, return Fools) , (1, return Twoo) ]

instance Semigroup Bull where
    (<>) _ _ = Fools

instance Monoid Bull where -- this is a false monoid!
    mempty = Fools

-- type BullMappend = Bull -> Bull -> Bull -> Bool

-- badMonoid :: IO ()
-- badMonoid = do
--     let ma  = prop_associative
--         mli = prop_monoidLeftId
--         mlr = prop_monoidRightId
--     quickCheck (ma :: BullMappend)
--     quickCheck (mli :: Bull -> Bool)
--     quickCheck (mlr :: Bull -> Bool)

-- Maybe Another Monoid

newtype First' a =
    First' { getFirst' :: Optional a }
    deriving (Eq, Show)

instance Arbitrary a => Arbitrary (First' a)
    where arbitrary = oneof
            [ pure $ First' Nada
            , First' . Only <$> arbitrary ]

instance Semigroup (First' a) where
    (<>) (First' Nada) x = x
    (<>) x             _ = x

instance Monoid (First' a) where
    mempty = First' Nada

-- firstMappend :: First' a -> First' a -> First' a
-- firstMappend = mappend

-- type FirstMappend = First' String -> First' String -> First' String -> Bool
-- type FstId = First' String -> Bool

-- Semigroups

instance Arbitrary a => Arbitrary (NonEmpty a) where
    arbitrary = do
        xs <- arbitrary
        x  <- arbitrary
        return $ x :| xs

main :: IO ()
-- main = putStrLn "hi"
main = hspec $ do
    describe "Optional monoid" $ do
        it "is associative" $
            -- property prop_assocOptional
            property $ prop_associative @(Optional String)
        it "has a left identity" $
            property $ prop_monoidLeftId @(Optional String)
        it "has a right identity" $
            property $ prop_monoidRightId @(Optional String)
    describe "Bull" $ do
        it "is associative" $
            property $ prop_associative @Bull
        it "does not have a left identity" $
            expectFailure $ prop_monoidLeftId @Bull
        it "does not have a right identity" $
            expectFailure $ prop_monoidRightId @Bull
    describe "First'" $ do
        it "is associative" $
            property $ prop_associative @(First' String)
        it "has a left identity" $
            property $ prop_monoidLeftId @(First' String)
        it "has a right identity" $
            property $ prop_monoidRightId @(First' String)
    describe "NonEmpty" $
        it "is associative" $
            property $ prop_associative @(NonEmpty Char)
