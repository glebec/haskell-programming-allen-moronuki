module TypeInference2 where

f x y = x + y + 3

-- compiles.
-- f :: Num a => a -> a -> a
