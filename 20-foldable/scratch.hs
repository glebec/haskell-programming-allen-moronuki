module ScratchCh20 where

import Data.Monoid
import Data.Foldable

xs = [1, 2, 3] :: [Sum Int]

x = fold xs -- Sum {getSum = 6}

-- the monoid instance doesn't influence foldr's func arg (unless you use (<>))
y = foldr (*) 5 (map Sum [1, 2, 3]) -- Sum {getSum = 30}

-- but it does matter for foldMap in the `mempty` case
fm1 = foldMap (+5) Nothing :: Sum Int -- Sum {getSum = 0}
fm2 = foldMap (+5) Nothing :: Product Int -- Product {getProduct = 1}

-- some example instances

newtype Identity a = Identity a deriving (Eq, Show)

instance Foldable Identity where -- notice the kind of Foldable's arg is * -> *
    foldMap f (Identity x) = f x

i1 = foldMap Sum (Identity 5) -- Sum {getSum = 5}

data Optional a = Nada | Yep a deriving (Eq, Show)

instance Foldable Optional where
    foldMap _ Nada    = mempty
    foldMap f (Yep a) = f a

o1 = foldMap (*3) Nada :: Sum Int -- Sum {getSum = 0}
o2 = foldMap Sum (Yep 2) -- Sum {getSum = 2}

-- some example operations

l1 = map toList [Just 1, Just 2, Nothing] -- [[1], [2], []]
l2 = concatMap toList [Just 1, Just 2, Nothing] -- [1, 2]
