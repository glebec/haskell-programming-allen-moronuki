{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

module Example_30_02_WhySomeException where

import Control.Exception (ArithException(..), AsyncException(..))
import Data.Typeable

-- data MyException = forall e . (Show e, Typeable e) => MyException e
data MyException where
    MyException :: (Show e, Typeable e) => e -> MyException

instance Show MyException where
    showsPrec p (MyException e) = showsPrec p e

-- seemingly non-polymorphic "MyException" left result…
multiError :: Int -> Either MyException Int
multiError n = case n of
    0 -> Left (MyException DivideByZero)  -- using an ArithException type…
    1 -> Left (MyException StackOverflow)  -- …and an AsyncException type.
    _ -> Right n

-- Enables extending `SomeException` without writing a sum type of all
-- the possibilities.
