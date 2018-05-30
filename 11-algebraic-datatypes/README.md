# 11. Algebraic Datatypes

"A type can be thought of as an enumeration of constructors that have zero or more arguments."

- `data` declarations
    - sum types
    - product types
        - positional syntax
        - record syntax
- type aliases (e.g. `String` for `[Char]`
- `newtype`, which provides for a different set of options and constraints from `data` or aliases

---

```hs
data MyType t1 = MyConstructor1 t1 String | MyConstructor2
```

Type constructors are only used at the type level (type signatures, typeclass definitions & instances).

Data constructors are used at the term level (values that can be interacted with at runtime).

Constants are nullary constructors (e.g. `Bool` type constant or `True` / `False` value constants).

Parameterized constructors need to be applied to yield a concrete type or value.
