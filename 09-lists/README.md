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
```
