module GreetIfCool2 where

greetIfCool :: (Ord a, Num a) => a -> IO ()
greetIfCool coolness =
    if isCool coolness
        then putStrLn "Eyyyy. What's shakin' bacon?"
    else
        putStrLn "Pshh."
    where isCool temp = temp < 65

-- personally I find the previous module cleaner.
