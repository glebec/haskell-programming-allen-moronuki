{-# OPTIONS_GHC -W #-}

module Hello (sayHello) where

sayHello :: String -> IO ()
sayHello name = putStrLn $ "Hello, " ++ name ++ "!"
