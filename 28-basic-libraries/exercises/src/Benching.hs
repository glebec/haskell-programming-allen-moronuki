module Benching where

import Criterion.Main

-- -- Report-adapted version (slow)
-- infixl 9 !?
-- (!?) :: (Ord t, Num t) => [a] -> t -> Maybe a
-- _      !? n | n < 0 = Nothing
-- []     !? _         = Nothing
-- (x:_)  !? 0         = Just x
-- (_:xs) !? n         = xs !? (n - 1)

-- optimized version
infixl 9 !?
{-# INLINABLE (!?) #-}
(!?) :: [a] -> Int -> Maybe a -- fast
-- (!?) :: (Ord t, Num t) => [a] -> t -> Maybe a -- slow
xs !? n
    | n < 0 = Nothing
    | otherwise =
        foldr
            (\x r k ->
                case k of
                    0 -> Just x
                    _ -> r (k - 1))
            (const Nothing) xs n

myList :: [Int]
myList = [1..9999]

benching :: IO ()
benching = defaultMain
    [ bench "index list 9999"
      $ whnf (myList !!) 9998
    , bench "index list maybe index 9999"
      $ whnf (myList !?) 9998
    ]
