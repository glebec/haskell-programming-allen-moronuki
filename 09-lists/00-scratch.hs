{-# OPTIONS_GHC -W #-}

module ScratchCh9 where

head' :: [a] -> Maybe a
head' []     = Nothing
head' (x:_) = Just x

acronym :: String -> String
acronym xs = [x | x <- xs, x `elem` ['A'..'Z']]
