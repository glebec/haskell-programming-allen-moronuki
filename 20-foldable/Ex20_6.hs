module Ex20_6 where

import Data.Monoid ((<>))

-- write `Foldable` instances

-- 1.
newtype Constant a b = Constant b

instance Foldable (Constant a) where
    foldMap f (Constant b) = f b

-- 2.
data Two a b = Two a b

instance Foldable (Two a) where
    foldMap f (Two a b) = f b

-- 3.
data Three a b c = Three a b c

instance Foldable (Three a b) where
    foldMap f (Three a b c) = f c

-- 4.
data Three' a b = Three' a b b

instance Foldable (Three' a) where
    foldMap f (Three' a b b') = f b <> f b'

-- 5.
data Four' a b = Four' a b b b

instance Foldable (Four' a) where
    foldMap f (Four' a b b' b'') = f b <> f b' <> f b''

-- write a filter using foldMap
filterF :: ( Applicative f
           , Foldable t
           , Monoid (f a) )
        => (a -> Bool) -> t a -> f a
filterF p = foldMap (\a -> if p a then pure a else mempty)
