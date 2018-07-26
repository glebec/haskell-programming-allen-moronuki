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
`Any`     | `False`   | `||`
`First`   | `Nothing` | take 1st `Just`
`Last`    | `Nothing` | take last `Just`

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
