{-# LANGUAGE TypeApplications #-}

module Main where

import Test.Hspec
import Test.QuickCheck
-- import Test.QuickCheck.Property.Generic (prop_Associative)

-- Exercise: Optional Monoid

data Optional a = Nada
                | Only a
                deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
    (<>) Nada _ = Nada
    (<>) _ Nada = Nada
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

main :: IO ()
-- main = putStrLn "hi"
main = hspec $
    describe "Optional monoid" $
        it "is associative" $
            -- property prop_assocOptional
            property (prop_associative @(Optional String))
