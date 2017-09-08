module Reverse where

main :: IO ()

rvrs :: String -> String
rvrs = unwords . foldl (flip (:)) [] . words

main = print $ rvrs "I want it that way"
