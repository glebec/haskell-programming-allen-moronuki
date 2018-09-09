# 20. Foldable

> "…a class of data structures that can be folded to a summary value."

> "The folding function is always dependent on some `Monoid` instance."

```hs
class Foldable (t :: * -> *) where
    fold    :: Monoid m => t m -> m
    foldMap :: Monoid m => (a -> m) -> t a -> m
    -- ...
    {-# MINIMAL foldMap | foldr #-}
```

Some operations:

```hs
null      :: Foldable t =>               t a -> Bool
length    :: Foldable t =>               t a -> Int
elem      :: (Foldable t, Eq a) =>  a -> t a -> Bool
maximum   :: (Foldable t, Ord a) =>      t a -> a -- unsafe
minimum   :: (Foldable t, Ord a) =>      t a -> a -- unsafe
sum       :: (Foldable t, Num a) =>      t a -> a -- safe
product   :: (Foldable t, Num a) =>      t a -> a -- safe
toList    :: Foldable t =>               t a -> [a]
concatMap :: Foldable t => (a -> [b]) -> t a -> [b]
```

Gotchas:

```hs
fmap length   Just [1, 2, 3] -- 1 -- (composes `length . Just`)
fmap length $ Just [1, 2, 3] -- Just 3
     length $ Just [1, 2, 3] -- 1 -- gets the `length` of a Maybe

fmap maximum [Just 1, Just 2] -- [1, 2]
fmap maximum $ Just [1, 2] -- Just 2
minimum $ Left 5 -- exception: empty structure
```
