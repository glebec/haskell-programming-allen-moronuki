> module Chapter4Exercises where


\section{4.3}

Given the following datatype:

> data Mood = Blah | Woot deriving Show

1. What is the type constructor, or name of this type?

    A: `Mood`

2. If the function requires a `Mood` value, what are the values you could possibly use?

    A: `Blah` or `Woot`

3. We are trying to write a function `changeMood` to change Chris’s mood instantaneously. It should act like `not` in that, given one value, it returns the other value of the same type. So far, we’ve written a type signature `changeMood :: Mood -> Woot`. What’s wrong with that?

    A: It should be:

> changeMood :: Mood -> Mood

4. Now we want to write the function that changes his mood. Given an input mood, it gives us the other one. Fix any mistakes and complete the function:

> changeMood Blah = Woot -- fixed from `ChangeMood Mood = Woot`
> changeMood    _ = Blah

5. [Confirm it works in GHCi]

    A: it does


\section{4.6}

Exercises: Find the Mistakes

> e1 = not True && True -- instead of `true`
> e2 = not (x == 6) where x = 2 -- instead of `=`
> e3 = (1 * 2) > 5 -- good as-is
> e4 = "Merry" > "Happy" -- instead of `[Merry]`
> e5 = [1, 2, 3] ++ [4] -- instead of `++ "look at me!"`


\section{4.9}

> awesome = ["Papuchon", "curry", ":)"]
> also = ["Quake", "The Simons"]
> allAwesome = [awesome, also]

`length` is a function that takes a list and returns a result that tells how many items are in the list.

1. What is the type signature of `length`?

    A: My answer: `[a] -> Int`. GHCi: `Foldable t => t a -> Int`

2. What are the results of the following expressions?

    a) `length [1, 2, 3, 4, 5]` -> 5
    b) `length [(1, 2), (2, 3), (3, 4)]` -> 3
    c) `length allAwesome` -> 2
    d) `length (concat allAwesome)` -> 5

3. Given what we know about numeric types and the type signature of `length`, look at these two expressions. One works and one returns an error. Determine which will return an error and why.

    a) `6 / 3`
    b) `6 / length [1, 2, 3]` -> originally thought this was an error because `(/)` would be applied to `length`, but operators have lower precedence than vanilla function application. The real problem is that `length` returns an `Int`, and `(/)` takes `Fractional` values.

4. How can you  x the broken code from the preceding exercise using a different division function/operator?

    A: `div`? That seems to work. Yep, `:t div -- Integral a => a -> a -> a`

5. What is the type of the expression `2 + 3 == 5`? What would we expect as a result?

    A: Bool, True

6. What is the type and expected result value of the following: `let x = 5`, `x + 3 == 5`.

    A: Bool, False

7. Below are some bits of code. Which will work? Why or why
not? If they will work, what value would these reduce to?

    a) `length allAwesome == 2` works: True
    b) `length [1, 'a', 3, 'b']` does not work: lists are singly-typed
    c) `length allAwesome + length awesome` works: 5
    d) `(8 == 8) && ('b' < 'a')` works: False
    e) `(8 == 8) && 9` does not work: `&&` only operates on Bool values

8. Write a function that tells you whether or not a given String (or list) is a palindrome. [use `reverse`]

> isPalindrome :: Eq a => [a] -> Bool
> isPalindrome l = l == reverse l

9. Write a function to return the absolute value of a number using if-then-else.

> myAbs :: Integer -> Integer
> myAbs n = if (n < 0) then (-n) else n

10. Fill in the de nition of the following function, using `fst` and `snd`:

> f :: (a, b) -> (c, d) -> ((b, d), (a, c))
> f t1 t2 = ((snd t1, snd t2), (fst t1, fst t2))

\subsection{Correcting Syntax}

In the following examples, you’ll be shown syntactically incorrect code. Type it in and try to correct it in your text editor, validating it with GHC or GHCi.

1. Here, we want a function that adds 1 to the length of a string argument and returns that result.

> x = (+)
> f2 xs = w `x` 1  -- corrected from`F` and 'x'
>        where w = length xs

2. This is supposed to be the identity function, `id`.

> id2 x = x -- corrected from '\X = x'

3. When fixed, this function will return `1` from the value `(1, 2)`.

> myFst (a, b) = a -- corrected from `myFst (a b) = A

\subsection{Match the function names to their types}

1. `show`

    a) `show a => a -> String` <- this one
    b) `Show a -> a -> String`
    c) `Show a => a -> String`

2. `==`

    a) `a -> a -> Bool`
    b) `Eq a => a -> a -> Bool` <- this one
    c) `Eq a -> a -> a -> Bool`
    d) `Eq a => A -> Bool`

3. `fst`

    a) `(a, b) -> a` <- this one
    b) `b -> a`
    c) `(a, b) -> b`

4. `(+)`

    a) `(+) :: Num a -> a -> a -> Bool`
    b) `(+) :: Num a => a -> a -> Bool`
    c) `(+) :: num a => a -> a -> a`
    d) `(+) :: Num a => a -> a -> a` <- this one
    e) `(+) :: a -> a -> a`
