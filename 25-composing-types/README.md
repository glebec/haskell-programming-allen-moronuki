# 25. Composing Types

This chapter is a gentle ramp-up to monad transformers. Some type tricks:

```hs
newtype Compose f g a =
    Compose { getCompose :: f (g a) }
    deriving (Eq, Show)
```

```
> :k Compose
Compose :: (* -> *) -> (* -> *) -> * -> *

> :t Compose [Just 'x']
Compose [Just 'x'] :: Compose [] Maybe Char
```

```hs
instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f (Compose fg) = Compose $ (fmap . fmap) f fg
```

## `IdentityT`

```hs
{-# LANGUAGE InstanceSigs #-}

newtype IdentityT f a =
    IdentityT { runIdentityT :: f a }
    deriving (Eq, Show)

instance Functor m => Functor (IdentityT m) where
    fmap :: (a -> b) -> IdentityT m a -> IdentityT m b
    fmap f (IdentityT fa) = IdentityT $ fmap f fa

instance Applicative m => Applicative (IdentityT m) where
    pure :: a -> IdentityT m a
    pure = IdentityT . pure

    (<*>) :: IdentityT m (a -> b) -> IdentityT m a -> IdentityT m b
    (<*>) (IdentityT ma) (IdentityT mx) = IdentityT $ ma <*> mx

instance Monad m => Monad (IdentityT m) where
    (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
    (>>=) (IdentityT ma) f = IdentityT $ ma >>= runIdentityT . f
```

The key bit above is the `runIdentityT` function to fold away the extra structure generated. There is no generic way to do this, it will be specific to each monad. Hence why we need a monad transformer for each monad.

```hs
impossibleFold :: (Monad m, Monad t) => m (t (m b)) -> m (m b)
impossibleFold = error "impossible without more information"
```
