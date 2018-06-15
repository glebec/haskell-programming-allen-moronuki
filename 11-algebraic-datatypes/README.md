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

---

_Kinds_ (`:kind` or `:k` in GHCi) are like the types of types. Signified using `*`.

- Concrete types have kind `*`.
- Single-param types have kind `* -> *`.

---

Phantom types:

```hs
data SomeType a = SomeData -- a is "phantom"
```

---

> _"Information about types does not persist through to runtime."_

…But then how can case / pattern matching work? Surely _some_ info is preserved. Matt Parsons answered me on Slack:

> Sum types are compiled into a pair of a machine word and the payload. The machine word indicates which branch to take. So `data Bool = False | True` compiles into `0` for False and `1` for True.
>
> `data [] a = [] | a : [a]` compiles `[]` into `0`, and `(:)` into `1, pointer-to-a, pointer-to-rest-of-list`.
> You can use the function `Unsafe.Coerce.unsafeCoerce` to trick GHC into interpreting the type however you want.
>
> ```
> λ> import Unsafe.Coerce
> λ> unsafeCoerce [] == False
> True
> λ> unsafeCoerce (undefined:undefined) == True
> True
> ```
>
> but, yeah, GHC doesn't keep type information at runtime. So there isn't a way to do something like:
>
> ```
> id :: a -> a
> id x = if x `instanceof` Int then 0 else x
> ```
>
> There is a typeclass, called `Typeable`, that lets you do this. But you rarely need it.

---

> "The difference between `newtype` and the type it contains is gone by the time the compiler generates the code."

Newtypes can only have a single unary data constructor.

```hs
-- both become identical to ints in the end
newtype Cows  = Cows  Int deriving (Eq, Show)
newtype Goats = Goats Int deriving (Eq, Show)
tooManyGoats (Goats n) = n > 42
```

> "One key contrast between a `newtype` and a type alias is that you can define typeclass instances for newtypes that differ from the instances for their underlying type."

We can use `GeneralizedNewtypeDeriving` to allow newtypes to reuse typeclass instances of their wrapped type.

---

Language pragma `FlexibleInstances` can be used to enable defining typeclass instances for things like tuples (otherwise you must use type variables).

```hs
instance TooMany (Int, String) where tooMany (a, _) = a > 42
```

---

Extension `NegativeLiterals` quashes spurious warnings about negative literals whose signficand exceeds the boundary of a bounded type, e.g. However, in my current version of GHC (8.2?), this doesn't seem to actually be needed.

---

Record syntax:

```hs
data Person =
  Person { name :: String
         , age  :: Int }
         deriving (Eq, Show)
-- Person "Houdini" 45
```

Deconstructing record values:

```hs
data FarmerRec =
  FarmerRec { name :: Name
            , acres :: Acres
            , farmerType :: FarmerType } deriving Show

isDairyFarmerRec :: FarmerRec -> Bool
isDairyFarmerRec farmer =
  case farmerType farmer of
    DairyFarmer -> True
    _           -> False
```

Don't use sums of records. Split the sum type and the record type into separate types. This prevents making the record accessor functions partial.

---

Type synonyms:

```hs
type AuthorName = String
```

---

Number of inhabitants for a function type `a -> b` is b^a. Example:

```hs
data Weave = Circle | Round | Thrice
data Eyes = Holy | Dread
data HoneyDew = Paradise (Weave -> Eyes)
```
