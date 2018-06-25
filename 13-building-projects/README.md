# 13. Building Projects

## Stack

Command       | Action
--------------|-------
`stack build` | build project from `stack.yaml`
`stack setup` | determine which GHC to use, based on LTS
`stack exec -- BIN_NAME` | execute linked `BIN_NAME`
`stack ghci`  | start a REPL with project modules loaded

If an `executable` stanza relies on module(s) from your `library` stanza, you can use the module name (in lowercase, apparently) in `build-deps`.

```cabal
executable hello
  hs-source-dirs:      exe
  main-is:             Main.hs
  default-language:    Haskell2010
  build-depends:       base >= 4.7 && < 5
                     , hello

library
  hs-source-dirs:      src
  exposed-modules:     Hello
  build-depends:       base >= 4.7 && < 5
  default-language:    Haskell2010
```

## Exports

```hs
module NameOfModule (export1, export2) where
```
