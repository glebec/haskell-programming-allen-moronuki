module Ex26_03 where

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

-- 1.

instance Functor m => Functor (EitherT e m) where
    fmap f (EitherT mEea) = EitherT $ (fmap . fmap) f mEea

-- 2.

instance Applicative m => Applicative (EitherT e m) where
    pure = EitherT . pure . pure
    (<*>) (EitherT mEea) (EitherT mEea') =
        EitherT $ fmap (<*>) mEea <*> mEea'

-- 3.

instance Monad m => Monad (EitherT e m) where
    (>>=) (EitherT mEea) f =
        EitherT $ mEea >>= either (pure . Left) (runEitherT . f)
-- (>>=) (EitherT mEea) f = EitherT $ do
--     x <- mEea
--     case x of
--         Left e -> pure $ Left e
--         Right a -> runEitherT $ f a

-- 4.

swapEither :: Either a b -> Either b a
swapEither = either Right Left

swapEitherT :: Functor m => EitherT e m a -> EitherT a m e
swapEitherT (EitherT mEea) = EitherT $ swapEither <$> mEea

-- 5.

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT aMc bMc (EitherT mEab) = mEab >>= either aMc bMc
-- eitherT cl cr (EitherT mEab) = do
--     e <- mEab
--     case e of
--         Left a -> cl a
--         Right b -> cr b
