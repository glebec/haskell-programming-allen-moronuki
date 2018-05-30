# 10. Folds

- `foldr (+) 0 [1, 2, 3]` -> `(1 + (2 + (3 + 0)))`
- `foldl (+) 0 [1, 2, 3]` -> `(((0 + 1) + 2) + 3)`

- `foldr` can work on infinite lists as the reducer can omit the remaining fold.
- `foldl` is not usually worth using.
- `foldl'` strictly evaluates as it goes, rather than building up a thunk.
