module Ex26_14 where

import Control.Monad.Identity
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad

-- 1.

rDec :: Num a => Reader a a
rDec = ReaderT $ \r -> Identity (r - 1)

-- 2.

rDec' :: Num a => Reader a a
rDec' = ReaderT $ Identity . subtract 1

rDec'' :: Num a => Reader a a
rDec'' = ReaderT $ subtract 1 . Identity -- Identity has a Num instance!

rDec''' :: Num a => Reader a a
rDec''' = reader $ subtract 1

-- 3. & 4.

rShow :: Show a => ReaderT a Identity String
rShow = ReaderT $ Identity . show

rShow' :: Show a => Reader a String
rShow' = reader show

-- 5.

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \r -> putStr "Hi: " >> print r >> pure (r + 1)

-- 6.

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT $ \s -> do
    let str = show s
    putStrLn $ "Hi: " ++ str
    pure (str, s + 1)

-- from https://github.com/andrewMacmurray/haskell-book-solutions/blob/f4fd386/src/ch26/Exercises.hs#L18,
-- a nice use of `liftIO`:

liftGreet :: (Show a, MonadIO m) => a -> m ()
liftGreet x = liftIO . putStrLn $ "hello " ++ show x

sPrintIncAccum' :: (Num a, Show a) => StateT a IO String
sPrintIncAccum' = StateT $ \s -> liftGreet s >> pure (show s, s + 1)

-- 7.

-- Fix the code, do not change types.

isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = do
    v <- liftIO getLine
    guard $ isValid v
    return v

doExcite :: IO ()
doExcite = do
    putStrLn "say something excite!"
    excite <- runMaybeT maybeExcite
    case excite of
        Nothing -> putStrLn "MOAR EXCITE"
        Just e -> putStrLn ("Good, was very excite: " ++ e)
