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
