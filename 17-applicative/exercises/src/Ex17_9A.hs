module Ex17_9A where

import Control.Applicative (liftA2, liftA3)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Prelude.Unicode

-- Specialize the method types
-- pureX = pure :: a -> ? a
-- apX   = (<*>) :: ? (a -> b) -> ? a -> ? b

-- 1. []
pure1 = pure  :: a -> [] a
ap1   = (<*>) :: [] (a -> b) -> [] a -> [] b

-- 2. IO
pure2 = pure  :: a -> IO a
ap2   = (<*>) :: IO (a -> b) -> IO a -> IO b

-- 3. (,) a
pure3 = pure  :: a -> (String, a)
ap3   = (<*>) :: (String, a -> b) -> (String, a) -> (String, b)

-- 4. (->) e
pure4 = pure  :: a -> (e -> a)
ap4   = (<*>) :: (e -> (a -> b)) -> (e -> a) -> e -> b

-- Write instances, use checkers to validate

data Pair   a       = Pair   a a     deriving (Eq, Show)
data Two    a b     = Two    a b     deriving (Eq, Show)
data Three  a b c   = Three  a b c   deriving (Eq, Show)
data Three' a b     = Three' a b b   deriving (Eq, Show)
data Four   a b c d = Four   a b c d deriving (Eq, Show)
data Four'  a b     = Four'  a a a b deriving (Eq, Show)

-- 1. Pair

instance Functor Pair where
    fmap f (Pair x x') = Pair (f x) (f x')

instance Applicative Pair where
    pure a = Pair a a
    (<*>) (Pair f f') (Pair a a') = Pair (f a) (f' a')

instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary = liftA2 Pair arbitrary arbitrary

instance Eq a => EqProp (Pair a) where (=-=) = eq

checkPair :: IO ()
checkPair = quickBatch $
    applicative ((⊥) :: Pair (String, Int, Int))

-- 2. Two

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
    pure = Two mempty
    (<*>) (Two a f) (Two a' x) = Two (a <> a') (f x)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = liftA2 Two arbitrary arbitrary

instance (Eq a, Eq b) => EqProp (Two a b) where
    (=-=) = eq

checkTwo :: IO ()
checkTwo = quickBatch $
    applicative ((⊥) :: Two String (String, Int, Int))

-- 3. Three

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
    pure = Three mempty mempty
    (<*>) (Three a b f) (Three a' b' x) = Three (a <> a') (b <> b') (f x)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
    Arbitrary (Three a b c) where
    arbitrary = liftA3 Three arbitrary arbitrary arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
    (=-=) = eq

checkThree :: IO ()
checkThree = quickBatch $
    applicative ((⊥) :: Three [Int] [Int] (String, Int, Int))

-- 4. Three'

instance Functor (Three' a) where
    fmap f (Three' a b b') = Three' a (f b) (f b')

instance Monoid a => Applicative (Three' a) where
    pure b = Three' mempty b b
    (<*>) (Three' a f f') (Three' a' x x') = Three' (a <> a') (f x) (f' x')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = liftA3 Three' arbitrary arbitrary arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where
    (=-=) = eq

checkThree' :: IO ()
checkThree' = quickBatch $
    applicative ((⊥) :: Three' [Int] (String, Int, Int))

-- 5. Four

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
    pure = Four mempty mempty mempty
    (<*>) (Four a b c f) (Four a' b' c' x) =
        Four (a <> a') (b <> b') (c <> c') (f x)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
    Arbitrary (Four a b c d) where
    arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
    (=-=) = eq

checkFour :: IO ()
checkFour = quickBatch $
    applicative ((⊥) :: Four [Int] [Int] [Int] (String, Int, Int))

-- 6. Four'

instance Functor (Four' a) where
    fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

instance Monoid a => Applicative (Four' a) where
    pure = Four' mempty mempty mempty
    (<*>) (Four' a1 a2 a3 f) (Four' a1' a2' a3' x) =
        Four' (a1 <> a1') (a2 <> a2') (a3 <> a3') (f x)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Four' a b) where
    (=-=) = eq

checkFour' :: IO ()
checkFour' = quickBatch $
    applicative ((⊥) :: Four' [Int] (String, Int, Int))

-- Combinations

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)

svs = (\(a, b, c) -> [a, b, c]) <$> combos stops vowels stops
