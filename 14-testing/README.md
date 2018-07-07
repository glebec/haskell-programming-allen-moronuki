# 14. Testing

You can run `stack init` on an existing `.cabal` file.

```hs
import Test.Hspec
import Test.QuickCheck
```

## Hspec

```hs
hspec "label" $ do
    describe "label" $ do
        it "does something" $ do
            val1 `shouldBe` val2
```

## QuickCheck

Function     | `::` | Example
-------------|------|--------
`sample`     | `Show a => Gen a -> IO ()` | `sample (arbitrary :: Gen Int)`
`sample'`    | `Gen a -> IO [a]` | `sample' (arbitrary :: Gen Int)`
`elements`   | `[a] -> Gen a` | `elements ['a'..'z']`: r, d, z, etc.
`choose`     | `System.Random.Random a => (a, a) -> Gen a` | `sample $ choose (False, True)`: T, F, F…
`frequency`  | `[(Int, Gen a)] -> Gen a` | `frequency [(1, return Nothing), (3, return (Just 5))]`
`quickCheck` | `Testable prop => prop -> IO ()` | `quickCheck (\x -> x + 1 > x)`

- generators
- `arbitrary`
- `elements`
- `choose`

### More Stack

- `echo "hello world" | stack exec morse-exe from`
- `stack test`
- `stack ghci morse:morse-test`
- edit `package.yaml` and run `stack build`
