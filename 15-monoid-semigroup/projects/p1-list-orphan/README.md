How to build modules so they can see other modules, without a Cabal file or similar:

```sh
ghc -I. --make ListyInstances.hs
```

(`-I` includes dir)
