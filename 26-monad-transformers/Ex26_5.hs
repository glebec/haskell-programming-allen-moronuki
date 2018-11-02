{-# LANGUAGE InstanceSigs #-}

module Ex26_5 where

newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

-- 1.

instance Functor m => Functor (StateT s m) where
    fmap :: (a -> b) -> StateT s m a -> StateT s m b
    fmap f (StateT s2Mas) = StateT $ fmap (\(a, s') -> (f a, s')) . s2Mas

-- 2.

-- Surprisingly, we need the monad constraint for applicative.
-- See https://stackoverflow.com/questions/18673525/is-it-possible-to-implement-applicative-m-applicative-statet-s-m
-- and https://github.com/NICTA/course/issues/134
instance Monad m => Applicative (StateT s m) where
    pure :: a -> StateT s m a
    pure x = StateT $ \s -> pure (x, s)

    (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
    (<*>) (StateT s2Mfs) (StateT s2Mxs) = StateT $ \s0 -> do
        (f, s1) <- s2Mfs s0
        (x, s2) <- s2Mxs s1
        pure (f x, s2)

-- 3.

instance Monad m => Monad (StateT s m) where
    (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
    (>>=) (StateT s2Mas) f = StateT $ \s0 -> do
        (a, s1) <- s2Mas s0
        runStateT (f a) s1
