module Main where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Test.Hspec

import BadMonad (checkBadMonad)

-- 18.7 Chapter Exercises

-- First exercise: write monad instances

checkFAM ::
    ( Monad m, CoArbitrary a, CoArbitrary b,
      EqProp (m a), EqProp (m b), EqProp (m c),
      Arbitrary a, Arbitrary b, Arbitrary c,
      Arbitrary (m a), Arbitrary (m b), Arbitrary (m c),
      Arbitrary (m (b -> c)), Arbitrary (m (a -> b)),
      Show a, Show (m a), Show (m (b -> c)), Show (m (a -> b))
    ) => m (a, b, c) -> IO ()
checkFAM a = do
    quickBatch $ functor a
    quickBatch $ applicative a
    quickBatch $ monad a

-- 1. Nope

data Nope a = Nopity deriving (Eq, Show)

instance Functor Nope where
    fmap _ _ = Nopity

instance Applicative Nope where
    pure _ = Nopity
    _ <*> _ = Nopity

instance Monad Nope where
    _ >>= _ = Nopity

instance Arbitrary (Nope a) where
    arbitrary = pure Nopity

instance EqProp (Nope a) where
    (=-=) = eq

checkNope :: IO ()
checkNope = checkFAM (undefined :: Nope (Int, Int, Int))
    -- let trigger :: Nope (Int, Int, Int)
    --     trigger = undefined
    -- quickBatch $ functor trigger
    -- quickBatch $ applicative trigger
    -- quickBatch $ monad trigger

-- 2. EitherOr

data EitherOr a b = Lefty a | Righty b deriving (Eq, Show)

instance Functor (EitherOr a) where
    fmap _ (Lefty a)  = Lefty a
    fmap f (Righty b) = Righty $ f b

instance Applicative (EitherOr a) where
    pure = Righty
    (<*>) (Righty f) (Righty a) = Righty $ f a
    (<*>) (Righty _) (Lefty b)  = Lefty b
    (<*>) (Lefty b)  _          = Lefty b

instance Monad (EitherOr a) where
    (Righty a) >>= f = f a
    (Lefty b)  >>= _ = Lefty b

instance (Eq a, Eq b) => EqProp (EitherOr a b) where
    (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (EitherOr a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        frequency [(3, pure $ Righty a), (1, pure $ Lefty b)]

checkEitherOr :: IO ()
checkEitherOr = checkFAM (undefined :: EitherOr Int (Int, Int, Int))

-- 3. Identity

newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
    pure = Identity
    (Identity f) <*> (Identity a) = Identity $ f a

instance Monad Identity where
    (Identity a) >>= f = f a

instance Eq a => EqProp (Identity a) where
    (=-=) = eq

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

checkIdentity :: IO ()
checkIdentity = checkFAM (undefined :: Identity (Int, Int, Int))

header :: String -> IO ()
header s = putStrLn $ "\n\n" ++ s

-- 4.

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Semigroup (List a) where
    Nil <> ys = ys
    Cons x xs <> ys = Cons x $ xs <> ys

instance Applicative List where
    pure x = Cons x Nil
    (<*>) (Cons f fs) xs = (f <$> xs) <> (fs <*> xs)
    (<*>) _ _ = Nil

instance Monad List where
    xs >>= f = concat' $ f <$> xs where
               concat' Nil = Nil
               concat' (Cons x xs) = x <> concat' xs

instance Eq a => EqProp (List a) where
    (=-=) = eq

genList :: Arbitrary a => Gen (List a)
genList = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return Nil), (3, return $ Cons a b)]

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = genList

checkList :: IO ()
checkList = checkFAM (undefined :: List (Int, Int, Int))

-- Second exercise: write definitions using functor & monad methods

-- 1.

join' :: Monad m => m (m a) -> m a
join' = (>>= id)

-- 2.

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = (<$>)
-- l1 f ma = ma >>= (pure . f)

-- 3.

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 abc ma mb = ma >>= \a -> mb >>= \b -> pure (abc a b)

-- 4.

a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)
-- a ma mab = ma >>= \a -> mab >>= \ab -> pure (ab a)

-- 5.

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh xs x2mb = go $ x2mb <$> xs where
              go :: Monad m => [m b] -> m [b]
              go [] = pure []
              go (mb:mbs) = mb >>= \b -> go mbs >>= \bs -> pure (b : bs)

-- 6.

flipType :: (Monad m) => [m a] -> m [a]
flipType xs = meh xs id

--

main :: IO ()
main = do
    header "Bad Monad"
    checkBadMonad
    header "OG List Monad"
    quickBatch $ monad ([(1, 2, 3)] :: [(Int, Int, Int)])
    header "Nopity Monad"
    checkNope
    header "EitherOr Monad"
    checkEitherOr
    header "Identity Monad"
    checkIdentity
    header "Custom List Monad"
    checkList
