module Ex17_8A_ZipList where

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
    -- mempty = pure mempty
    mempty = ZipList $ repeat mempty

-- -- already defined in Test.QuickCheck
-- instance Arbitrary a => Arbitrary (ZipList a) where
--     arbitrary = ZipList <$> arbitrary

instance Eq a => EqProp (ZipList a) where
    (=-=) = eq
