module Tree where

import Test.QuickCheck
import Test.QuickCheck.Checkers

data Tree a = Empty
            | Leaf a
            | Node (Tree a) a (Tree a)
            deriving (Eq, Show)

instance Functor Tree where
    fmap _ Empty = Empty
    fmap f (Leaf a) = Leaf $ f a
    fmap f (Node ta a ta') = Node (fmap f ta) (f a) (fmap f ta')

instance Foldable Tree where
    foldMap _ Empty = mempty
    foldMap f (Leaf a) = f a
    foldMap f (Node ta a ta') = foldMap f ta <> f a <> foldMap f ta'

instance Traversable Tree where
    sequenceA Empty = pure Empty
    sequenceA (Leaf a) = Leaf <$> a
    sequenceA (Node ta a ta') = Node <$> sequenceA ta <*> a <*> sequenceA ta'

instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = do
        a   <- arbitrary
        ta  <- arbitrary
        ta' <- arbitrary
        frequency [ (1, pure Empty)
                  , (1, pure $ Leaf a)
                  , (1, pure $ Node ta a ta') ]

instance Eq a => EqProp (Tree a) where
    (=-=) = eq
