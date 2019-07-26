# 28. Basic Libraries

- Compile with `-O` or `-O2` to optimize (`stack ghc -- -O2 bench.hs`).
- Benchmarking with `criterion`.

## Profiling

See [here](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html) for more info.

### Time

```sh
stack ghc -- -prof -fprof-auto -rtsopts -O2 whatever.hs # compile

./whatever +RTS -P # run and generate profile

cat whatever.prof # view profile results
```

- `-prof`: profile
- `-fprof-auto`: assign all bindings an SCC (cost center)
- `-rtsopts`: pass GHC RTS options to the binary
- `+RTS`: runtime system option flag
- `-P`: detailed time report (`-p` is apparently more common)

### Heap

```sh
./whatever +RTS -hc -p # run and generate profile

hp2ps whatever.hp # convert hp -> ps file
```

- `-hc`: breaks down graph by cost-centre stack
- `hp2ps`: utility for converting `hp` files to PostScript

## Constant Applicative Forms (CAFs)

> _…expressions that have no free variables and are held in memory to be shared with all other expressions in a module._

## `containers`

- `Map`: a binary tree using ordered keys
- `Set`: same as `Map` but without the vals
- `HashMap` / `IntMap` / `Vector`: faster DSs using `Int` keys
- `Seq`: finger tree, cheap front/back append

## `vector`

- `Vector`: sliced array, can be boxed (pointers) or unboxed (if Bool / Char / newtype of such / etc.); see `//` for batch updating
- Mutable vectors, `Control.Monad.ST`

```sh
stack --work-dir .stack-work-profile build --profile --ghc-options -O2

stack --work-dir .stack-work-profile exec -- benching +RTS -hc
```

- `--work-dir .stack-work-profile` avoid recompilation between artifacts

## `String`, `Text`, `ByteText`

- `text`, `bytestring`, `utf8-string` libraries
- frequently used with `{-# LANGUAGE OverloadedStrings #-}`
- `unpack` to convert Text to String, `pack` for vice-versa
- do not use `char8`
