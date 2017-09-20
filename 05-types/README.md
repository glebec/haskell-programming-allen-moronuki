# 5. Types

## Misc

Haskell is statically typed, but also uses Hindley-Milner type inference.

The compiler assigns the most general typeclass it can until forced to use a more specific type. For example `1 :: Num p => p`.

Polymorphic: can be one of any type. Constrained polymorphism: can be one of multiple specific types.

The arrow `->` is the type constructor for functions.

Can use `::` not only in type signatures but also to assign types in expressions, e.g. `(myNumVar :: Int)` vs. `(myNumVar :: Integer)`.

You can mix polymorphic values if they share a concrete intersection (e.g. `someDouble + someNum`) but you cannot mix concretely typed values for certain ops (e.g. `someDouble + someInt`).

Multiple constraints use parens:

```hs
f1 :: (Num a, Num b) => a -> b -> b
f2 :: (Ord a, Num a) => a -> a -> Ordering
```

You can check the types of things that aren’t implemented yet, so long as you give GHCi an `undefined` to bind the signature to.

```hs
f = undefined :: a -> a -> a -> a
x = undefined :: Char
:t f x -- f x :: Char -> Char -> Char
```

## Currying

All functions in Haskell are both automatically curried and can be partially applied (one of my favorite aspects of the language). As in the lambda calculus, function abstraction is right-associative: `a -> b -> c` is the same as `a -> (b -> c)` (NB, function application is left associative: `f a b` is `(f a) b`).

You can manually uncurry functions by making a function take a tuple of arguments. You can manually curry uncurried functions by defining a new function which applies arguments extracted from a tuple.

```hs
curry f a b = f (a, b)
uncurry f (a, b) = f a b
curry fst 1 2 -- 2
uncurry (+) (1, 2) -- 3
```

## Lambdas

Haskell has a lambda syntax. To illustrate, the K combinator can be expressed as `\a b -> a`.

## Sectioning

Partial application of infix operators.

```hs
inc = (+1)
yell = (++ "!")
from5 = (5-)
```

Can also be used with prefix functions using backticks:

```hs
isSingleDigit = (`elem` [0..9])
```

