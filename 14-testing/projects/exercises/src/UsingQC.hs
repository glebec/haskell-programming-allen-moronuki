module UsingQC where

-- 1.

half :: Fractional a => a -> a
half x = x / 2

-- 2.

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
    snd $ foldr go (Nothing, True) xs
    where go _ status@(_, False) = status
          go y (Nothing, t)      = (Just y, t)
          go y (Just x, t)       = (Just y, x >= y)
