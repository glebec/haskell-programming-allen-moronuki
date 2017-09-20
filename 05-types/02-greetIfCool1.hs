module GreetIfCool1 where

greetIfCool :: (Ord a, Num a) => a -> IO ()
greetIfCool coolness =
    if cool
        then putStrLn "Eyyyy. What's shakin' bacon?"
    else
        putStrLn "Pshh."
    where cool = coolness < 65
