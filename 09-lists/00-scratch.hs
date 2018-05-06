{-# OPTIONS_GHC -W #-}

module ScratchCh9 where

head' :: [a] -> Maybe a
head' []     = Nothing
head' (x:xs) = Just x
