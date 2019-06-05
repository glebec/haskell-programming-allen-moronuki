{-# LANGUAGE InstanceSigs #-}

module BookEx where

import Control.Monad.Trans.Class

-- provided by book / practiced again on my own

-- 26.2 MaybeT

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Functor m => Functor (MaybeT m) where
    fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance Applicative m => Applicative (MaybeT m) where
    pure x = MaybeT (pure . pure $ x)
    (MaybeT fab) <*> (MaybeT mma) = MaybeT $ (<*>) <$> fab <*> mma

instance Monad m => Monad (MaybeT m) where
    (MaybeT mma) >>= f = MaybeT $ do
        ma <- mma
        case ma of
            Just a -> runMaybeT $ f a
            Nothing -> pure Nothing

instance MonadTrans MaybeT where
    lift :: Monad m => m a -> MaybeT m a
    lift = MaybeT . fmap pure

-- 26.4 ReaderT

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where
    fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma

instance Applicative m => Applicative (ReaderT r m) where
    pure x = ReaderT (pure . pure $ x)
    (ReaderT rmf) <*> (ReaderT rmx) = ReaderT $ (<*>) <$> rmf <*> rmx

instance Monad m => Monad (ReaderT r m) where
    (ReaderT rma) >>= f = ReaderT $ \r -> do
        a <- rma r
        runReaderT (f a) r

instance MonadTrans (ReaderT r) where
    lift :: Monad m => m a -> ReaderT r m a
    lift = ReaderT . const
