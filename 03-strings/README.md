# 3. Strings

Strings are lists of `Char`. `"Hello" :: [Char]`.

Some commands:

* `print :: Show a => a -> IO ()`
* `putStrLn :: String -> IO ()`
* `putStr :: String -> IO ()`

Tip, have heard that for big blocks of text one should use `Data.Text`, not strings.

## Main

`main` is the default _action_ to perform when loading a module or executing a binary. We generate an IO monad, which serves as a sort of runtime recipe.

In a full Stack project, we'd put `main` in a `Main.hs` file.

It seems that for `IO ()` ops you use `>>` rather than `>>=` to chain. Or you can use `do` notation. (_Addendum: interesting to look back on these notes years later and spot misunderstandings like this; `>>` is of course sequencing monads without using the prior result, while `>>=` is normal monadic bind which can access the prior result._)

## Concatenation

* `concat :: Foldable t => t [a] -> [a]`
* `(++) :: [a] -> [a] -> [a]`

Example:

```hs
myGreeting = concat ["hello", " ", "world"]
```

This is different from the book's stated type of `concat` (given as `[[a]] -> [a]`), which is from version < 7.10.

## Scoping

Top-level (module-level) definitions are in scope everywhere and do not have to be declared in order. They are also exported / imported to other modules.

Local definitions are created using `let` and `where`.

## More lists

* `(:) :: a -> [a] -> [a]` (aka "cons") constructs lists
* `take :: Int -> [a] -> [a]` (safe)
* `drop :: Int -> [a] -> [a]` (safe)
* `head :: [a] -> a` (_unsafe!_)
* `tail :: [a] -> [a]` (_unsafe!_)
* `(!!) :: [a] -> Int -> a` (_unsafe!_)

## Etc

Literate Haskell files end in `.lhs` and can use bird track style (`>`) or TeX style (`begin{code}`).
