# 21. Traversable

- sequencing: swap structure layers (foldable functor <-> applicative)
- traversal: map elements to applicative, then sequence

```hs
class (Functor t, Foldable t) => Traversable t where
    traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
    traverse f = sequenceA . fmap f
    sequenceA :: Applicative f => t (f a) -> f (t a)
    sequenceA = traverse id
    -- ...
    {-# MINIMAL traverse | sequenceA #-}
```

- `traverse` is more general/useful than `mapM`.
- `sequenceA` is more general/useful than `sequence`.

> "So, `traverse` is just `fmap` and the `Traversable` version of `sequence` bolted together into one convenient function. `sequence` is the unique bit, but you need to do the `fmap`  first most of the time, so you end up using `traverse`. This is very similar to the way `>>=` is just `join` composed with `fmap` where `join` is the bit that is unique to `Monad`."

## Some Instances

```hs
instance Traversable (Either a) where
    traverse _ (Left x)  = pure (Left x)
    traverse f (Right y) = Right <$> f y

instance Traversable ((,) a) where
    traverse f (x, y) = (,) x <$> f y
```

## Laws

- Naturality: `t . traverse f = traverse (t . f)`. Transforming the final outer layer is the same as transforming the produced structure on the inside before swapping the types.
- Identity: `traverse Identity = Identity`. Traversable doesn't add or inject any structure / context.
- Composition: `traverse (Compose . fmap g . f) = Compose . fmap (traverse g) . traverse f`. "…we can collapse sequential traversals into a single traversal, by taking advantage of the `Compose` datatype, which combines structure."

### Misc / Asides

- [Vectors](http://hackage.haskell.org/package/vector)
