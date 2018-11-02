# 26. Monad Transformers

For some unknown monad `m`, a monad transformer for a known monad `t` is a datatype `m t a` with functor / applicative / monad instances.

- The "inner" monad is the known one, so we know how to "get at" the `a`.
- The "outer" monad is unknown, so all we can do is use generic functions on it.
- The hard-coded transformer part is only needed for the `Monad` instance.

```hs
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

-- map the `a` "inside" the m-t stack
instance Functor m => Functor (MaybeT m) where
    fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

-- apply an `f` "inside" an m-t stack
instance Applicative m => Applicative (MaybeT m) where
    pure = MaybeT . pure . pure
    (<*>) (MaybeT mtf) (MaybeT mta) = MaybeT $ fmap (<*>) mtf <*> mta

-- the `m` and `t` don't change, but the `a` does
instance Monad m => Monad (MaybeT m) where
    (>>=) (MaybeT mta) f = MaybeT $ do
        ta <- mta
        case ta of -- the unique part of the transformer
            Nothing -> pure Nothing
            Just x -> runMaybeT (f x)
```

- we need a transformer for each `t` type as there is no generic way to compose two monads.
