{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Function
import Prelude.Unicode
import ChapterExercises

functorIdentity :: ∀ f a . (Eq (f a), Functor f) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: ∀ f c a b . (Eq (f c), Functor f)
               => f a -> Fun a b -> Fun b c -> Bool
functorCompose x (Fun _ f) (Fun _ g) =
    fmap (g ∘ f) x == (fmap g ∘ fmap f $ x)

-- 16.10

-- 1.

newtype Identity a = Identity a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

-- 2.

data Pair a = Pair a a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary = Pair <$> arbitrary <*> arbitrary

instance Functor Pair where
    fmap f (Pair a b) = Pair (f a) (f b)

-- 3.

data Two a b = Two a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = Two <$> arbitrary <*> arbitrary

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

-- 4.

data Three a b c = Three a b c deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c)
    => Arbitrary (Three a b c) where
    arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

-- 5.

data Three' a b = Three' a b b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

instance Functor (Three' a) where
    fmap f (Three' a b b') = Three' a (f b) (f b')

-- 6.

data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
    => Arbitrary (Four a b c d) where
    arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

-- 7.

data Four' a b = Four' a a a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Functor (Four' a) where
    fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

-- 8.

-- cannot implement for Trivial because its kind is TYPE, not TYPE -> TYPE.

-- 16.11

data Possibly a = LolNope
                | Yeppers a
                deriving (Eq, Show)

instance Functor Possibly where
    fmap _ LolNope = LolNope
    fmap f (Yeppers a) = Yeppers $ f a

data Sum a b = First a
             | Second b
             deriving (Eq, Show)

instance Functor (Sum a) where
    fmap _ (First a)  = First a
    fmap f (Second b) = Second $ f b

main :: IO ()
-- main = undefined
main = hspec $
    describe "Functor instances" $ do
        describe "Identity" $ do
            it "obeys the identity law" $
                property $ functorIdentity @Identity @Int
            it "obeys the composition law" $
                property $ functorCompose @Identity @Int @Int @Int
        describe "Pair" $ do
            it "obeys the identity law" $
                property $ functorIdentity @Pair @Int
            it "obeys the composition law" $
                property $ functorCompose @Pair @Int @Int @Int
        describe "Two" $ do
            it "obeys the identity law" $
                property $ functorIdentity @(Two Int) @Int
            it "obeys the composition law" $
                property $ functorCompose @(Two Int) @Int @Int @Int
        describe "Three" $ do
            it "obeys the identity law" $
                property $ functorIdentity @(Three Int Int) @Int
            it "obeys the composition law" $
                property $ functorCompose @(Three Int Int) @Int @Int @Int
        describe "Three'" $ do
            it "obeys the identity law" $
                property $ functorIdentity @(Three' Int) @Int
            it "obeys the composition law" $
                property $ functorCompose @(Three' Int) @Int @Int @Int
        describe "Four" $ do
            it "obeys the identity law" $
                property $ functorIdentity @(Four Int Int Int) @Int
            it "obeys the composition law" $
                property $ functorCompose @(Four Int Int Int) @Int @Int @Int
        describe "Four'" $ do
            it "obeys the identity law" $
                property $ functorIdentity @(Four' Int) @Int
            it "obeys the composition law" $
                property $ functorCompose @(Four' Int) @Int @Int @Int
