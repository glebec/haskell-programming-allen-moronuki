module Scratch13 where

twoo :: IO Bool
twoo = do c  <- getLine
          c' <- getLine
          return (c == c')

-- main :: IO ()
-- main = do
--     b <- twoo
--     if b then putStrLn "Yup same"
--          else putStrLn "Different"

main = twoo >>= \p -> putStrLn $ if' p "Yup same" "Different"

if' :: Bool -> a -> a -> a
if' True  a _ = a
if' False _ b = b
