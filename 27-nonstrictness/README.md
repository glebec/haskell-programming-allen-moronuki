# 27. Nonstrictness

- truly lazy languages memoize function applications
- nonstrict evaluation does not specify if results are memoized or not

## Haskell, Core Etc.

Case & pattern matching forces the value up to the level you are matching on (because there's no other way to tell which case to return).

```hs
-- does not bottom out in Haskell
case undefined of { _ -> False}

-- does bottom out in Core
case undefined of { DEFAULT -> False }
```

## WHNF

Weak Head Normal Form: stop at the first data constructor or lambda.

```hs
a = (x, undefined) -- WHNF (constructor is 2-tuple)
x = \_ -> undefined -- WHNF (lambda)
b = x + undefined -- not WHNF (no lambda or constructor)
```

## `seq`

In core, case expressions force evaluation, and `seq` generates a case expression if the second argument is used. So `seq` can be used to create a dependency that forces evaluation.

```hs
seq :: a -> b -> b
-- implementation elided
result1 = a `seq` x -- a is forced when x is forced
result2 = undefined `seq` y `seq` x -- forcing x forces y forces undefined
```

Note that if the value isn't used, the program does not bottom out:

```hs
noProblem :: Int
noProblem =
    let x = undefined
        y = 2
        z = (x `seq` y `seq` 10, 11)
    in snd z -- evaluates fine
```

This example will throw, and before printing "hi":

```hs
hypo'' :: IO ()
hypo'' = do
    let x :: Integer
        x = undefined
    s <- x `seq` getLine -- throws here
    case s of
        "hi" -> print x
        _ -> putStrLn "hello"
```
