module Example_30_07 where

import Control.Exception

-- data NotDivThree = NotDivThree deriving (Eq, Show)
-- instance Exception NotDivThree

-- data NotEven = NotEven deriving (Eq, Show)
-- instance Exception NotEven

newtype NotDivThree = NotDivThree Int deriving (Eq, Show)
instance Exception NotDivThree

newtype NotEven = NotEven Int deriving (Eq, Show)
instance Exception NotEven

evenAndThreeDiv :: Int -> IO Int
evenAndThreeDiv i
    | rem i 3 /= 0 = throwIO $ NotDivThree i
    | odd i = throwIO $ NotEven i
    | otherwise = pure i
