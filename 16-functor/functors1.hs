module Functors1 where

data FixMePls = FixMe | Pls deriving (Eq, Show)

instance Functor FixMePls where
    fmap = error "it does not matter, will not compile"

-- Expected kind ‘* -> *’, but ‘FixMePls’ has kind ‘*’
