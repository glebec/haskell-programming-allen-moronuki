# 9. Lists

```hs
safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (x:[]) = Nothing
safeTail (_:xs) = Just xs
```

---

Ranges:

- `[1..10]` becomes `enumFromTo 1 10`
- `[2, 4..10]` becomes `enumFromThenTo 2 4 10` -> `[2,4,6,8,10]`
- `[1..]` becomes `enumFrom`
- `[1, 3..]` becomes `enumFromThen`
- `['t'..'z']` (so, anything enumerable)

`enumFromTo a b` must have `a < b`.

---

```hs
take      :: Int         -> [a] -> [a]
drop      :: Int         -> [a] -> [a]
splitAt   :: Int         -> [a] -> ([a], [a])
takeWhile :: (a -> Bool) -> [a] -> [a]
dropWhile :: (a -> Bool) -> [a] -> [a]
elem      :: (Eq a, Foldable t) => a -> t a -> Bool
```

---

Comprehensions:

- `[x^2 | x <- [1..10]]`
- `[x^2 | x <- [1..10], rem x 2 == 0]`
- `[x^y | x <- [1..5], y <- [2, 3]]` is 1, 1, 4, 8 and so on.
- `[x^y | x <- [1..5], y <- [2, 3], x^y < 200]`
- `[(x, y) | x <- [1, 2, 3], y <- ['a', 'b']]` is (1, a), (1, b), (2, a) etc.
- `[x | x <- "Three Letter Acronym", elem x ['A'..'Z']]` is "TLA"

---

`:sprint` command can indicate (somewhat) what expressions have been evaluated.

```
λ: let blah = enumFromTo 'a' 'z'
λ: :sprint blah
blah = _
λ: take 3 blah
"abc"
λ: sprint blah
blah = 'a' : 'b' : 'c' : _
```

**WHNF** (Weak Head Normal Form) – evaluated to at least a data constructor.

- `(1, 2)` WHNF & NF
- `(1, 1 + 1)` WHNF (not NF)

`length` is strict in the spine but not the values. Using `_` in pattern matching can be used to ignore values.

`take 2 $ map (+1) [1, 2, undefined]` prints `[2, 3]`.

---

From `Data.Bool`:

- `bool :: a -> a -> Bool -> a`

From `Data.Char`:

- `chr :: Int -> Char`
- `ord :: Char -> Int`

---

Surprising import issue: do not forget that type names and constructor names can be identical. Importing a type does not automatically import the type's constructors.

```hs
import Data.Monoid -- everything, including `Any`
import Data.Monoid (Any) -- imports the Any **TYPE** (but not constructor(s)!)
import Data.Monoid (Any(..)) -- imports the Any type with all of its constructors
import Data.Monoid (Any(Any)) -- imports the Any type with just the Any constructor
```
