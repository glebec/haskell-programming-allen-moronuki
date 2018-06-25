{-# OPTIONS_GHC -W #-}

module Hello (sayHello) where

sayHello :: IO ()
sayHello = putStrLn "hello world"
