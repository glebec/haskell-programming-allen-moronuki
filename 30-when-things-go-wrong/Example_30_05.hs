{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Example_30_05 where

import Control.Exception

canICatch :: forall t e . (Exception t, Exception e)
          => e
          -> IO (Either t ()) -- NOTE the t arg
canICatch e = try $ throwIO e  -- `try` creates the Either, `throwIO` throws e

works1 = canICatch @ArithException DivideByZero   -- Left divide by zero
fails1 = canICatch @ArithException StackOverflow  -- *** Exception

fails2 = canICatch @AsyncException DivideByZero   -- *** Exception
works2 = canICatch @AsyncException StackOverflow  -- Left stack overflow

alwaysWorks1 = canICatch @SomeException DivideByZero  -- Left divide by zero
alwaysWorks2 = canICatch @SomeException StackOverflow  -- Left stack overflow

-- The point: the exceptions `try` can catch depends on the types
