module Ex21_12 where

import Data.Proxy (Proxy(..))
import Data.Monoid ((<>))
import Test.QuickCheck
import Classes
import Data.Functor.Classes

-- Chapter Exercises

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

instance Foldable Identity where
    foldMap f (Identity a) = f a

instance Traversable Identity where
    traverse f (Identity a) = Identity <$> f a

-- using `quickcheck-classes` over `checkers`

instance Eq1 Identity where
    liftEq f (Identity a) (Identity a') = f a a'

instance Show1 Identity where
    liftShowsPrec sp _ d (Identity x) = showsUnaryWith sp "Identity" d x

instance Arbitrary1 Identity where
    liftArbitrary g = Identity <$> g

checkTrIdentity :: IO ()
checkTrIdentity = lawsCheck $ traversableLaws (Proxy :: Proxy Identity)

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
    fmap f (Constant a) = Constant a

instance Foldable (Constant a) where
    foldMap _ (Constant a) = mempty

instance Traversable (Constant a) where
    traverse _ (Constant a) = pure $ Constant a

-- add traversable checks once QuickCheck.Classes is updated

data Optional a = Nada | Yep a deriving (Eq, Ord, Show)

instance Functor Optional where
    fmap _ Nada = Nada
    fmap f (Yep a) = Yep $ f a

instance Foldable Optional where
    foldMap _ Nada = mempty

instance Traversable Optional where
    traverse _ Nada = pure Nada
    traverse f (Yep a) = Yep <$> f a

-- add traversable checks once QuickCheck.Classes is updated

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Foldable List where
    foldMap _ Nil = mempty
    foldMap f (Cons x xs) = f x <> foldMap f xs

-- instance Traversable List where
--     traverse _ Nil = pure Nil
--     traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs

instance Traversable List where
    sequenceA Nil = pure Nil
    sequenceA (Cons x xs) = Cons <$> x <*> sequenceA xs

-- add traversable checks once QuickCheck.Classes is updated

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
    foldMap f (Three a b c) = f c

instance Traversable (Three a b) where
    sequenceA (Three a b c) = Three a b <$> c

-- add traversable checks once QuickCheck.Classes is updated

data Pair a b = Pair a b deriving (Eq, Show)

instance Functor (Pair a) where
    fmap f (Pair a b) = Pair a (f b)

instance Foldable (Pair a) where
    foldMap f (Pair a b) = f b

instance Traversable (Pair a) where
    sequenceA (Pair a b) = Pair a <$> b

-- add traversable checks once QuickCheck.Classes is updated

data Big a b = Big a b b deriving (Eq, Show)

instance Functor (Big a) where
    fmap f (Big a b b') = Big a (f b) (f b')

instance Foldable (Big a) where
    foldMap f (Big a b b') = f b <> f b'

instance Traversable (Big a) where
    sequenceA (Big a b b') = Big a <$> b <*> b'

-- add traversable checks once QuickCheck.Classes is updated

data Bigger a b = Bigger a b b b deriving (Eq, Show)

instance Functor (Bigger a) where
    fmap f (Bigger a b b' b'') = Bigger a (f b) (f b') (f b'')

instance Foldable (Bigger a) where
    foldMap f (Bigger a b b' b'') = f b <> f b' <> f b''

instance Traversable (Bigger a) where
    sequenceA (Bigger a b b' b'') = Bigger a <$> b <*> b' <*> b''

-- add traversable checks once QuickCheck.Classes is updated
