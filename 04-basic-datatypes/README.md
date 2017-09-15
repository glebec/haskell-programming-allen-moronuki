# 4. Basic Datatypes

Term | Definition | Example(s)
-----|------------|-----------
Datatype | aka "type"; similar to set | `Bool`, `Int`, `Maybe`
Data Declaration | definition of a datatype, consisting of a type constructor and associated data constructors; use `:i` in GHCi | `data Noise = Bark \| Moo`
Type Constructor | name of the type (must be capitalized) | `Bool`, `Int`
Data Constructor | a value which inhabits a type | `False`, 3, `GT`
Type Signature | annotation for function input(s) and output, consisting of typeclasses, type constructors, and arrows; use `:t` in GHCi | `(Ord a, Num a) => a -> IO ()`
Typeclass | defined later, but as I understand it – types which share common functionality (e.g. all types which are functors support mapping) | `Eq`, `Num`, `Ord`
Pattern Matching | defining a function by matching on data constructor | `myFunc True = False`
Sum Type | Disjunction of possible values | `False \| True`
Product Type | Conjunction of possible values | `Just a`

At the *type level* we deal with type constructors. At the *term level* we deal with data constructors.

## Data Declarations

Declarations consist of:

* the `data` keyword
* the type constructor (e.g. `Bool`)
* `=`
* data constructor(s), separated by `|` (e.g. `False | True`)

```hs
data Bool = False | True
```

## Numeric Types (members of `Num` typeclass)

`Num` requires support for `*`, `+`, `-`, etc.

It would be possible to actually define a datatype for integers ourselves, using infinite recursive data constructors, but this would be very inefficient so GHC of course uses some shortcuts / hardware-based math.

* `Integral`
    * `Int`: two's complement fixed-precision (generally avoided, but can be higher-perf). Some related types, `Int8`–`Int64`, via `import GHC.Int`.
    * `Integer`: arbitrary precision (generally preferred)
* `Fractional`
    * `Float`: single-precision float (avoid in most cases)
    * `Double`: double-precision float (avoid in most cases)
    * `Rational`: ratio of two integers, abitrarily precise but not as efficient as `Scientific`
    * `Scientific`: space efficient and almost arbitrary precision, with `Integer` coefficient and `Int` exponent; from [a specific lib](https://hackage.haskell.org/package/scientific)

## Bools

* `not`
* `&&`, `||`
* `if-then-else` expressions are built-in syntax using booleans

## Tuples

* Built-in syntax `(,)`
* Arity: number of elements (interesting that arity would be used for tuples in addition for functions…)
* Can be mixed types
* `fst` / `snd` for 2-tuples
* `import Data.Tuple`, `swap (1, 2) -- (2, 1)`

## Lists

* Built-in syntax `[]`
* `++` for two lists
* `concat` for list of lists

## Names and Vars

Seven categories of things with names:

* Functions, e.g. `concat`
* Term-level variables, e.g. `myStr`
* Data constructors, e.g. `True`
* Type variables
* Type constructors, e.g. `Bool`

Can use unicode, including `'` (e.g. `f'`).

## Misc

* `/=`for inequalities.
* `compare` to return orderings (`LT`, `EQ`, `GT`)
* `<` can be used on lists in which each element can be compared
* resolved types of expressions can be abstract, i.e. typeclasses: `:t 100 -- Num a => a`
