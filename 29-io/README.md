# 29. IO

Nothing really new until the exercises.

## File I/O

Book suggestions:

```haskell
System.Environment.getArgs :: IO [String]
System.IO.hWaitForInput :: Handle -> Int -> IO Bool
System.IO.hPutStr  :: Handle -> String -> IO () -- or just use `putStr`
System.IO.hGetChar :: Handle -> IO Char -- or just use `getChar`
System.IO.stdout   :: Handle
System.IO.stdin    :: Handle
System.IO.stderr   :: Handle
```

Additional helpers that are useful:

```haskell
System.IO.isEOF :: IO Bool
System.Exit.die :: String -> IO a
System.Exit.exitSuccess :: IO a
Control.Monad.forever :: Applicative f => f a -> f b
Control.Monad.when :: Applicative f => Bool -> f () -> f ()
```

Or one of the "easy" ways:

```haskell
interact :: (String -> String) -> IO () -- process STDIN -> STDOUT
getContents :: IO String -- all contents until EOF
System.IO.hGetContents :: Handle -> IO String -- same as above
```
