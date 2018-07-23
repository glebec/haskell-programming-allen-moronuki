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
`oneof`      | `[Test.QuickCheck.Gen.Gen a] -> Test.QuickCheck.Gen.Gen a` | `oneof [return $ Just 5, return Nothing]`
`quickCheck` | `Testable prop => prop -> IO ()` | `quickCheck (\x -> x + 1 > x)`

Generators: It isn't shown in this chapter, but based on Doug B.'s "Monad Challenges" it would seem a `Gen` is a function from seed to value & new seed.

```hs
data Gen a = Seed -> (a, Seed)
```

- `arbitrary`: a generator of values in the `Arbitrary` type class.
- `coarbitrary`: a function which can be used to "vary" a generator. Apparently you can get instances of `CoArbitrary` for free by `deriving (Generic)` (using the `DeriveGeneric` language extension). This lets you make random functions? I am not following this yet.

```hs
arbitrary :: Arbitrary a => Gen a
coarbitrary :: CoArbitrary a => a -> Gen b -> Gen b
```

### More Stack

- piping to STDIN: `echo "hello world" | stack exec morse-exe from`
- `stack test`
- `stack ghci morse:morse-test`
- edit `package.yaml` and run `stack build`
