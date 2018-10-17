module Ex25_6 where

import Ex25_4

-- 1.

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
    foldMap h (Compose fgm) = foldMap (foldMap h) fgm

-- 2.

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
    traverse h (Compose fga) = Compose <$> traverse (traverse h) fga

-- 3.

class Bifunctor p where
    {-# MINIMAL bimap | first, second #-}

    bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
    bimap f g = first f . second g

    first :: (a -> b) -> p a c -> p b c
    first f = bimap f id

    second :: (b -> c) -> p a b -> p a c
    second = bimap id

-- 3.1

data Deux a b = Deux a b

instance Bifunctor Deux where
    bimap f g (Deux a b) = Deux (f a) (g b)

-- 3.2

newtype Const a b = Const a

instance Bifunctor Const where
    bimap f _ (Const a) = Const $ f a

-- 3.3

data Drei a b c = Drei a b c

instance Bifunctor (Drei a) where
    bimap f g (Drei a b c) = Drei a (f b) (g c)

-- 3.4

data SuperDrei a b c = SuperDrei a b

instance Bifunctor (SuperDrei a) where
    bimap f _ (SuperDrei a b) = SuperDrei a (f b)

-- 3.5

newtype SemiDrei a b c = SemiDrei a

instance Bifunctor (SemiDrei a) where
    bimap _ _ (SemiDrei a) = SemiDrei a

-- 3.6

data Quadriceps a b c d = Quadzzz a b c d

instance Bifunctor (Quadriceps a b) where
    bimap f g (Quadzzz a b c d) = Quadzzz a b (f c) (g d)

-- 3.7

data LR a b = Lefty a | Righty b

instance Bifunctor LR where
    bimap f _ (Lefty a) = Lefty $ f a
    bimap _ g (Righty b) = Righty $ g b
