# 26. Monad Transformers

```hs
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Functor m => Functor (MaybeT m) where
    fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance Applicative m => Applicative (MaybeT m) where
    pure x = MaybeT $ (pure . pure) x
    (<*>) (MaybeT fab) (MaybeT mma) = MaybeT $ fmap (<*>) fab <*> mma

instance Monad m => Monad (MaybeT m) where
    (>>=) (MaybeT ma) f = MaybeT $ do
        a <- ma
        case a of
            Nothing -> pure Nothing
            Just x -> runMaybeT (f x)
```
