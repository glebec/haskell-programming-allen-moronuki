# 16. Functor

A nice aligned snippet showing how functor `fmap` is elevated function application:

```hs
(<$>) :: Functor f => (a -> b) -> f a -> f b
($)   :: Functor f => (a -> b) ->   a ->   b
```

## 16.4 Exercises

1. kind of `a` in `a -> a` is `*`
2. kind of `b` and `T` in `a -> b a -> T (b a)` are both `* -> *`
3. kind of `c` in `c a b -> c b a` is `* -> * -> *`

---

`RankNTypes`, `Rank2Types`, or `ExplicitForAll` can be used to quantify type variables that would otherwise be considered out of scope in GHC:

```hs
{-# LANGUAGE RankNTypes #-}

-- a type for Natural Transformations between structures f and g
type Nat f g = forall a . f a -> g a
-- the `forall a` prevents `a` from being manipulated in the transformation

maybeToList :: Nat Maybe []
maybeToList Nothing  = []
maybeToList (Just a) = [a]
```

---

> "In Haskell, Functor instances will be unique for a given datatype."

