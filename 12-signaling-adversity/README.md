# 12. Signaling Adversity

"Smart" constructors are factory functions which provide data validation or other logic, wrapping around the raw data constructors.

```hs
type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

mkPerson :: Name -> Age -> Maybe Person
mkPerson name age
  | name /= "" && age >= 0 = Just $ Person name age
  | otherwise              = Nothing
```

---

> "Case expressions and pattern matching will work without an `Eq` instance, but guards using (`==`) will not."

---

> "kind `*` is the kind of all standard lifted types, while types that have the kind `#` are unlifted. A lifted type, which includes any datatype you could define yourself, is any that can be inhabited by _bottom_."

---

Hm. A type like `data Unary a = Unary a deriving Show` doesn't enforce that `a` is of type `Show a`. You can create `Unary id` without complaint. That is mildly disappointing.

---

List comprehensions can use pattern matching, e.g. `lefts xs = [x | Left x <- xs]`.

---

### Unfolds

- `iterate :: (a -> a) -> a -> [a]`
  - `iterate (+1) 4` [4, 5, 6…]
- `unfoldr :: (b -> Maybe (a, b)) -> b -> [a]`
  - `unfoldr (\x -> Just (x, x + 1)) 4` [4, 5, 6...]
  - left of tuple is the new seed; right the list value; Maybe whether to stop.
