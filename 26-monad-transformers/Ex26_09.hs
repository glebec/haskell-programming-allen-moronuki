{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

module Ex26_09 (module Ex26_05) where

import Control.Monad.Trans.Class
import Ex26_03 (EitherT(..))
import Ex26_05 (StateT(..))

-- newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance MonadTrans (EitherT e) where
    lift :: Monad m => m a -> EitherT e m a
    lift = EitherT . fmap Right

instance MonadTrans (StateT s) where
    lift :: Monad m => m a -> StateT s m a
    lift ma = StateT $ \s -> (,s) <$> ma
