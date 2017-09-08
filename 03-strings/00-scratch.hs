module ScratchCh3 where

-- Can do this in the REPL, but not in a module:
-- print "Hello"
-- putStrLn "Hello"
-- putStr "Hello"

-- without `do` notation (see 02-print.hs):
main :: IO ()
main =    putStrLn "One, two, three four five"
       >> putStr   "Everybody in the car"
       >> putStr   ", so come on "
       >> putStrLn "let's ride"
