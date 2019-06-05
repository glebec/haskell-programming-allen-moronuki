{-# LANGUAGE InstanceSigs #-}

module Ex26_10 where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import BookEx (MaybeT(..), ReaderT(..))
import Ex26_09 (StateT(..))

instance MonadIO m => MonadIO (MaybeT m) where
    liftIO :: IO a -> MaybeT m a
    liftIO = lift . liftIO

instance MonadIO m => MonadIO (ReaderT r m) where
    liftIO :: IO a -> ReaderT r m a
    liftIO = lift . liftIO

instance MonadIO m => MonadIO (StateT s m) where
    liftIO :: IO a -> StateT s m a
    liftIO = lift . liftIO
