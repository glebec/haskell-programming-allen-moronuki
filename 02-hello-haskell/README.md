# 2. Hello, Haskell

## Some common REPL-specific commands

* `:quit`, `:q`
* `:load`, `:l` (with no arguments, unloads)
* `:reload`, `:r`
* `:module`, `:m` (unloads)
* `:type`, `:t`
* `:info`, `:i` (includes associativity, precedence 0–9 increasing)
* `:kind`, `:k` (for type vars)

## Reduction

Reducible expressions are also called *redexes*.

Haskell uses WHNF (Weak Head Normal Form) evaluation.

## Operators, Associativity, Precadence, etc.

`id` is built in. So is `flip`, `const`, and `.`.

Book spec's operators as infix functions. Researching, the definition of "operator" varies across contexts. In general it is used most often for functions with "special" syntax or behavior, by some definition of "special".

If a function name in Haskell is alphanumeric, it is prefix by default. You can make prefix functions infix using backticks, and you can make infix functions prefix using sectioning.

`^` is a nice example of a right-associative operator: `2 ^ 3 ^ 2` is 512, not 64.

Op     | Name      | Purpose
-------|-----------|---
`/`    | slash     | fractional division
`div`  | divide    | integral division, round down (preferred?)
`quot` | quotient  | integral division, round towards zero
`mod`  | modulo    | remainder after modular division (wraps)
`rem`  | remainder | remainder after division

* `(quot x y) * y + (rem x y) == x`
* `(div x y) * y + (mod x y) == x`

In Haskell, the results of `mod` will have the same sign as the divisor, while the result of `rem` will have the same sign as the dividend.

`$` is infixr with precedence 0, useful for forcing evaluation of the right side before application to a function. `negate 2 + 5` vs. `negate $ 2 + 5`.

Function application has the highest precedence, it would seem – even higher than composition `.` (9). Here is a [useful fixity reference](https://www.haskell.org/onlinereport/decls.html#fixity).

Sectioning & partial function application, e.g. `(+1) 5`. For subtraction, you can use `(4-)` but not `(-4)` — use `(subtract 4)`. `(-4) == negate 4`.

## Expressions, Declarations, Formatting

`let` creates an expression, `where` is bound to some syntactic construct.

The book emphasizes using spaces rather than tabs for indentation. They also stress not indenting entire modules but rather beginning declarations at column zero.

`let` expressions can bind multiple values, separated by a semicolon. `let x = 4; y = 3 in x + y`, for example.
