# 27. Nonstrictness

## Tools

- `seq` to create evaluation dependencies
- `:sprint` in GHCi
- `import Debug.Trace` -> `trace :: String -> a -> a`

This results in two traces:

```hs
howManyTimes' =
    let onePlusOne =
        trace "I got eval'd" (1 + 1)
    in (2 + onePlusOne) + (3 + onePlusOne)
```

## Laziness

- truly lazy languages memoize function applications
- nonstrict evaluation does not specify if results are memoized or not

Term          | Definition
--------------|-----------
Call by value | argument expressions evaluated before entering function, expressions for bindings evaluated before creating binding. AKA strict.
Call by name  | expressions can be arguments to a function or targets for binding without evaluating. Includes nonstrict.
Call by need  | Same as call by name, but expressions only evaluated once.

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

This example will throw, but only after the `getLine` happens:

```hs
hypo' :: IO ()
hypo' = do
    let x :: Integer
        x = undefined
    s <- getLine
    case x `seq` s of -- throws after getLine
        "hi" -> print x
        _ -> putStrLn "hello"
```

This example will throw, and before the `getLine` happens:

```hs
hypo'' :: IO ()
hypo'' = do
    let x :: Integer
        x = undefined
    s <- x `seq` getLine -- throws before getLine
    case s of
        "hi" -> print x
        _ -> putStrLn "hello"
```

## Refutable vs. irrefutable patterns

Matching on `_` or a variable like `a` is irrefutable. Lazy patterns via `~` are also irrefutable. They are useful for unpacking products that might not get used.

```hs
lazyPattern :: (a, b) -> String
lazyPattern ~(a, b) = const "Cousin It" a
```

## `BangPatterns`

```hs
{-# LANGUAGE BangPatterns #-}

-- forced arguments

banging :: Bool -> Int
banging !b = 1 -- basically same as: banging b = b `seq` 1

-- strict data

data Foo = Foo Int !Int -- evals 2nd arg when Foo is eval'd to WHNF

first, second :: Foo -> Int
first  (Foo x _) = x
second (Foo _ y) = y

second (Foo undefined 1) -- OK
second (Foo 1 undefined) -- throws
first  (Foo 1 undefined) -- throws
```

## `Strict` / `ScrictData`

Basically applies `seq` to everything in the module.

```hs
{-# LANGUAGE Strict #-}

module LazyInHostileTerritory where

willForce x = 1
willNotForce ~x = 1
```
