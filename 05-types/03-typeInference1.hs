module TypeInference1 where

f :: Num a => a -> a -> a
f x y = x + y + 3

-- :t f 1
-- f 1 :: Num a => a -> a
