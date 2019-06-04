{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Control.Monad.Trans.Class
import Web.Scotty.Internal.Types (ActionT(..))
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy hiding (get)

liftReaderT :: m a -> ReaderT r m a
liftReaderT ma = ReaderT $ const ma

liftStateT :: Monad m => m a -> StateT s m a
liftStateT ma = StateT $ \s -> do
    a <- ma
    pure (a, s)

main :: IO ()
main = scotty 3000 $
    get "/:word" $ do
        beam <- param "word"
        let logIt = print beam
        (ActionT
            . (ExceptT . fmap Right)
            . liftReaderT
            . liftStateT :: IO () -> ActionM ()) logIt
        html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

-- lift :: (Monad m, MonadTrans t) => m a -> t m a
-- lift :: (MonadTrans t) => IO a -> t IO a
-- lift :: IO a -> ActionM a
-- lift :: IO () -> ActionM ()
-- lift = ActionT . lift . lift . lift
-- lift = ActionT . (ExceptT . liftM Right) . lift . lift
-- lift = ActionT . (ExceptT . liftM Right) . liftReaderT . lift
-- lift = ActionT . (ExceptT . liftM Right) . liftReaderT . liftStateT
