{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ExplicitForAll #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Test.Hspec
import Test.QuickCheck

prop_associative :: (Semigroup a, Eq a) => a -> a -> a -> Bool
prop_associative a b c = (a <> b) <> c == a <> (b <> c)

prop_leftId :: (Monoid a, Eq a) => a -> Bool
prop_leftId a = mempty <> a == a

prop_rightId :: (Monoid a, Eq a) => a -> Bool
prop_rightId a = a == a <> mempty

-- Semigroups

-- 1.

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = Trivial

instance Arbitrary Trivial where
    arbitrary = return Trivial

-- 2.

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
    Identity i <> Identity i' = Identity $ i <> i'

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

-- 3.

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    Two a b <> Two a' b' = Two (a <> a) (b <> b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = Two <$> arbitrary <*> arbitrary

-- 4.

data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) =>
    Semigroup (Three a b c) where
        Three a b c <> Three a' b' c' =
            Three (a <> a') (b <> b') (c <> c')

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
    Arbitrary (Three a b c) where
        arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

-- 5.

data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) =>
    Semigroup (Four a b c d) where
        Four a b c d <> Four a' b' c' d' =
            Four (a <> a') (b <> b') (c <> c') (d <> d')

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
    Arbitrary (Four a b c d) where
    arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

-- 6.

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
    (<>) (BoolConj True) (BoolConj True) = BoolConj True
    (<>) _ _ = BoolConj False

instance Arbitrary BoolConj where
    arbitrary = BoolConj <$> arbitrary

-- 7.

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
    (<>) (BoolDisj False) (BoolDisj False) = BoolDisj False
    (<>) _ _ = BoolDisj True

instance Arbitrary BoolDisj where
    arbitrary = BoolDisj <$> arbitrary

-- 8.

data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
    (Fst _) <> (Fst b) = Fst b
    s@(Snd _) <> _     = s
    _ <> s             = s

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        elements [Fst a, Snd b]

-- 9.

-- run both functions, (<>) the outputs
newtype Combine a b = Combine { unCombine :: a -> b }

instance Semigroup b => Semigroup (Combine a b) where
    Combine f <> Combine g = Combine (f <> g) -- \a -> f a <> g a

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
    arbitrary = Combine <$> arbitrary

prop_combAssoc :: forall a b . (Eq b, Semigroup b) =>
    Blind (Combine a b) -> -- Blind removes the need for Show
    Blind (Combine a b) ->
    Blind (Combine a b) ->
    a -> Bool
prop_combAssoc (Blind c) (Blind c') (Blind c'') a =
    unCombine ((c <> c') <> c'') a ==
    unCombine (c <> (c' <> c'')) a

-- type BCmbIntStr = Blind (Combine Int String)
-- type ExComb = BCmbIntStr -> BCmbIntStr -> BCmbIntStr -> Int -> Bool

-- 10.

newtype Comp a = Comp { unComp :: a -> a }

instance Semigroup (Comp a) where
    Comp f <> Comp g = Comp $ f . g

instance (Arbitrary a, CoArbitrary a) => Arbitrary (Comp a) where
    arbitrary = Comp <$> arbitrary

prop_compAssoc :: forall a . Eq a =>
    Blind (Comp a) ->
    Blind (Comp a) ->
    Blind (Comp a) ->
    a -> Bool
prop_compAssoc (Blind c) (Blind c') (Blind c'') a =
    unComp ((c <> c') <> c'') a ==
    unComp (c <> (c' <> c'')) a

-- 11.

data Validation a b = Failure' a | Success' b deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
    s@(Success' _) <> _           = s
    Failure' a     <> Failure' a' = Failure' $ a <> a'
    _              <> f           = f

success :: Int -> Validation String Int
success = Success'

failure :: String -> Validation String Int
failure = Failure'

-- specs

main :: IO ()
main = hspec $ do
    describe "Trivial" $
        it "is associative" $
            property $ prop_associative @Trivial
    describe "Identity" $
        it "is associative" $
            property $ prop_associative @(Identity String)
    describe "Two" $
        it "is associative" $
            property $ prop_associative @(Two Ordering [Int])
    describe "Three" $
        it "is associative" $
            property $ prop_associative @(Three Ordering [Int] (Either Int Int))
    describe "Four" $
        it "is associative" $
            property $ prop_associative @(Four Ordering [Int] (Either Int Int) String)
    describe "BoolConj" $
        it "is associative" $
            property $ prop_associative @BoolConj
    describe "BoolDisj" $
        it "is associative" $
            property $ prop_associative @BoolDisj
    describe "Or" $
        it "is associative" $
            property $ prop_associative @(Or String Int)
    describe "Combine" $
        it "is associative" $
            property $ prop_combAssoc @Int @String
    describe "Comp" $
        it "is associative" $
            property $ prop_compAssoc @Int
    describe "Validation" $ do
        it "takes the first success over later failure" $
            success 1 <> failure "blah" `shouldBe` success 1
        it "combines failures" $
            failure "woot" <> failure "blah" `shouldBe` failure "wootblah"
        it "takes the first success over later success" $
            success 1 <> success 2 `shouldBe` success 1
        it "takes the first success over earlier failure" $
            failure "woot" <> success 2 `shouldBe` success 2
