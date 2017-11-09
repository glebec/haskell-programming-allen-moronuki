# 6. Typeclasses

Some typeclass examples.

* `Eq`
* `Ord`
* `Num`
* `Enum`
* `Show`

Can use `:i` to get information about a typeclass.

Classes are defined using `class`:

```hs
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
```

Some classes can be derived via `deriving (Typeclass1, Typeclass2)`:

* `Eq`
* `Ord` (so long as you also have an `Eq`)
* `Enum`
* `Bounded`
* `Read`
* `Show`

```hs
data Example = Foo | Bar deriving (Eq, Ord, Show)
```

Type instances of typeclasses can be defined using `instance`:

```hs
-- instance Typeclass Type where <method> Data = <result>
instance Eq Mytype where
    Mydata == Mydata = True

-- instance Constraints => Typeclass (Type typevar) where <method> Data = <result>
instance Eq t => Eq (Pair t) where
    Pair a b == Pair a' b' = a == a' && b == b'
```

Types are populated using `data`:

```hs
data DayOfWeek =
    Mon | Tue | Weds | Thu | Fri | Sat | Sun
```

Typeclasses can inherit:

```hs
class Num a => Fractional a where
    (/) :: a -> a -> a
    recip :: a -> a
    fromRational :: Rational -> a
```

## Totality

Prevent compiling partial functions using `Wall` (alternatively `-W` for less noise).

* As pragma: `{-# OPTIONS_GHC -Wall #-}`
* As REPL config: `:set -Wall`
* As CLI flag: `-Wall`

## Num

Interesting result of `:i Integral`: `class (Real a, Enum a) => Integral a where`. Any integral type must also have real & enum instances. Real also needs num & ord: `class (Num a, Ord a) => Real a where`.

## Ord

Must implement `compare` (returning `LT`, `EQ`, or `GT`).

## Enum

Some methods: `enumFromTo`, `enumFromThenTo`.
