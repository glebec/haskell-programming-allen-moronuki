module Functors2 where

-- actually Functors3, didn't bother to make a new file

data FixMePls a = FixMe | Pls a deriving (Eq, Show)

instance Functor FixMePls where
    fmap _ FixMe   = FixMe
    fmap f (Pls a) = Pls (f a)
