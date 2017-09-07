-- random chapter snippets / experiments

module ScratchCh2 where

sayHello :: String -> IO ()
sayHello x = putStrLn ("Hello, " ++ x ++ "!")

classic :: IO ()
classic = sayHello "World"

triple :: Integer -> Integer
triple = (*3)
-- triple 4 -> 12

three = negate 2 + 5
negSeven = negate $ 2 + 5
