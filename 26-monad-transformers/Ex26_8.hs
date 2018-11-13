module Ex26_8 where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader

-- lexically outer = semantically inner
embedded :: MaybeT -- m (Maybe a)
                (ExceptT -- m
                    String
                    (ReaderT () IO))
                Int -- a
embedded = pure 1

maybeUnwrap :: ExceptT -- m (Either e a)
                   String -- e
                   (ReaderT () IO) -- m
                   (Maybe Int) -- a
maybeUnwrap = runMaybeT embedded

eitherUnwrap :: ReaderT () IO -- r -> m a
                (Either -- a
                    String
                    (Maybe Int))
eitherUnwrap = runExceptT maybeUnwrap

readerUnwrap :: () -> -- r
                IO -- m
                    (Either -- a
                        String
                        (Maybe Int))
readerUnwrap = runReaderT eitherUnwrap

unwrappedResult :: IO (Either String (Maybe Int))
unwrappedResult = readerUnwrap () -- Right (Just 1)

unwrapped2 :: IO (Either String (Maybe Int))
unwrapped2 = (runReaderT . runExceptT . runMaybeT . pure $ 1) ()

-- Exercise: Wrap It Up

-- NB, the book seems to contradict itself. It says we should
-- re-wrap `readerUnwrap`, but also provides the following snippet:
-- `embedded = ??? (const (Right (Just 1)))`. Here are two approaches:

-- matches written problem description
embedded' :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded' = MaybeT . ExceptT . ReaderT $ readerUnwrap

-- matches provided code snippet
embedded'' :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded'' = MaybeT . ExceptT . ReaderT . fmap pure $ const (Right (Just 1))
