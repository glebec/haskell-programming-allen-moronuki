# 15. Monoid, Semigroup

- set + binary op which…
  - is closed
  - is associative
  - has an identity

Classes can provide default implementations for functions (which may have more efficient implementations defined per type):

```hs
class Monoid m where
    mempty  :: m
    mappend :: m -> m -> m
    mconcat :: [m] -> m
    mconcat = foldr mappend mempty
```

Some common monoid instances (some in `Data.Monoid`).

Instance  | Identity  | Operation
----------|-----------|----------
`[a]`     | `[]`      | `(++)`
`Sum`     | 0         | `(+)`
`Product` | 1         | `(*)`
`All`     | `True`    | `&&`
`Any`     | `False`   | `¦¦`*
`First`   | `Nothing` | take 1st `Just`
`Last`    | `Nothing` | take last `Just`

* `¦` substituted for `|` because of markdown table formatting limitations

## Newtype Aside

Reasons to use `newtype`:

- signal intent ("cannot eventually grow into a more complicated sum or product type")
- improve type safety
- add different typeclass instances (à la `Sum` or `Product` above)

### Orphaned Typeclass Instances

> "An orphan instance is when an instance is defined for a datatype and typeclass, but not in the same module as either the declaration of the typeclass or the datatype. …writing orphan instances should be avoided _at all costs_."

Solutions:

- if you defined the type, add the instance in the type module
- if you defined the typeclass, add the instance in that module
- if neither, create a `newtype` wrapper and give it an instance

## Misc

You can bind params with infix names:

```hs
\ (<>) a b c ->
    a <> (b <> c) == (a <> b) <> c
```

---

`verboseCheck` is a version of `quickCheck` which reports which values were tested.

---

One legit-seeming use for `type` aliases is to have shorter type annotations:

```hs
type Short = Type1 -> Type2 -> Type1 -> Type3 -> Type1

a = func1 :: Short
b = func2 :: Short
```

Another way to deal with this is the `TypeApplications` extension, which lets you explicitly set the type argument(s) to a compiled function:

```hs
someFunc :: (Eq a) => a -> a -> a -> a -> Int -> Bool
someFunc = ...

-- customized = someFunc :: String -> String -> String -> String -> Int -> Bool
customized = someFunc @String
```

---

> "Data constructors with only nonalphanumeric symbols and that begin with a colon are infix by default."

The above lets you distinguish between infix functions and infix data constructors.

```hs
data NonEmpty a = a :| [a] deriving (Eq, Ord, Show)
```
