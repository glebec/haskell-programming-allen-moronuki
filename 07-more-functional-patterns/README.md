# 7. More Functional Patterns

Applying a function to a value not only binds its parameter to the value, but its type variable to a type.

```hs
id :: a -> a
id a = a

-- a :: Num t => t
id 5 == 5
```

IIRC, in certain programming languages based on lambda calculus, types are represented as unique values (e.g. Church numerals) in one half of a pair (i.e. the V combinator), with the other half being the value itself. Is this how System F / System FC / Haskell works under the hood?

---

`let` expressions introduce a scope. They can also introduce shadowed variables.

---

Haskell's lambda syntax uses `\` and `->`.

```hs
foldr (\a b -> a + b * b) 0 nums
```

---

Patterns can be matched against values or data constructors (but not types).

`_` matches everything.

Order from most to least specific.

Can prevent compilation of partial functions with `{-# OPTIONS_GHC -Wincomplete-patterns -Werror #-}`.

---

`newtype` permits only one data constructor and only one field. That means it is useful as an alias for another type. For example, when you want to create functions that don't accept any old `Int` but rather an `Int` that represents something specific (e.g. `AccountNumber`), you can use `newtype`.

```hs
newtype AccountNumber = AccountNumber Integer
```

---

`case` expressions can have a `where` clause.

```hs
pal' xs =
  case y of
    True -> "yes"
    False -> "no"
  where y = xs == reverse xs
```

---

Guards come between the function definition and `=` sign. They can also have `where` clauses.

```hs
avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | y >= 0.9   = 'A'
  | y >= 0.8   = 'B'
  | y >= 0.7   = 'C'
  | y >= 0.595 = 'D'
  | y <  0.595 = 'F'
  | otherwise  = error "How did you even get here?"
  where y = x / 100
```

---

- Function composition `(.)` is `infixr 9`.
- Function application is precedence 10.
- `$` is `infixr 0`.
