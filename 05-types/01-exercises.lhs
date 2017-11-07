> {-# LANGUAGE NoMonomorphismRestriction #-}

> module Chapter5Exercises where

\section{5.3}

Type Matching

Match each function to its type signature.

Function | Signature
---------|----------
`not`    | `:: Bool -> Bool`
`length` | `:: [a] -> Int`
`concat` | `:: [[a]] -> [a]`
`head`   | `:: [a] -> a`
`(<)`    | `:: Ord a => a -> a -> Bool`


\section{5.4}

Type Arguments

Given a function and its type, tell us what type results from applying some or all of the arguments.

> f :: a -> a -> a -> a;                   f = undefined
> x :: Char;                               x = undefined
> g :: a -> b -> c -> b;                   g = undefined
> h :: (Num a, Num b) => a -> b -> b;      h = undefined
> jackal :: (Ord a, Eq b) => a -> b -> a;  jackal = undefined
> kessel :: (Ord a, Num b) => a -> b -> a; kessel = undefined

1. `f x :: Char -> Char -> Char`
2. `g 0 'c' "woot" :: Char`
3. `h 1.0 2 :: Num b => b` (I originally said `Integral`… ah, I see.)
4. `h 1 (5.5 :: Double) :: Double`
5. `jackal "keyboard" "has the word jackal in it" :: [Char]`
6. `jackal "keyboard" :: Eq b => b -> [Char]`
7. `kessel 1 2 :: (Num a, Ord a) => a`
8. `kessel 1 (2 :: Integer) :: (Num a, Ord a) => a`
9. `kessel (1 :: Integer) 2 :: Integer`


\section{5.5}

Parametricity

1. Given the type a -> a, which is the type for id, attempt to make a function that terminates successfully that does something other than returning the same value. This is impossible, but you should try it anyway.

2. We can get a more comfortable appreciation of parametricity by looking at `a -> a -> a`. This hypothetical function `a -> a -> a`` has two—and only two—implementations. Write both possible versions of `a -> a -> a`. After doing so, try to violate the constraints of parametrically polymorphic values we outlined above.

> monoK :: a -> a -> a
> monoK a b = a
> monoKI :: a -> a -> a
> monoKI a b = b

3. Implement `a -> b -> b`. How many implementations can it have? Does the behavior change when the types of 𝑎 and 𝑏 change?

> ki :: a -> b -> b
> ki a b = b


\section{5.6}

Given an unapplied function (for which compiler will infer maximally polymorphic type) and an applied function, figure out how the type would change and why.

1. `(++) :: [a] -> [a] -> [a]`

> myConcat x = x ++ " yo"

myConcat :: [Char] -> [Char]

2. `(*) :: Num a => a -> a -> a`

> myMult x = (x / 3) * 5

myMult :: Fractional a => a -> a

3. take :: Int -> [a] -> [a]

> myTake x = take x "hey you"

myTake :: Int -> [Char]

4. (>) :: Ord a => a -> a -> Bool

> myCom x = x > (length [1..10])

myCom :: Int -> Bool

5. (<) :: Ord a => a -> a -> Bool

> myAlph x = x < 'z'

myAlph :: Char -> Bool


\section{5.8}

Chapter Exercises

---

Multiple choice:

1. A value of type [a] is
    c) a list whose elements are all of some type 𝑎
2. A function of type [[a]] -> [a] could
    a) take a list of strings as an argument
3. A function of type [a] -> Int -> a
    b) returns one element of type 𝑎 from a list
4. A function of type (a, b) -> a
    c) takes a tuple argument and returns the first value

---

For the following functions, determine the type of the specified value.

1. All function applications return a value. Determine the value returned by these function applications and the type of that value.

> e1a = (* 9) 6 -- 54 :: Num a => a
> e1b = head [(0,"doge"),(1,"kitteh")] -- (0, "doge") :: Num a => (a, [Char])
> e1c = head [(0 :: Integer ,"doge"),(1,"kitteh")] -- (0, "doge") :: (Integer, [Char])
> e1d = if False then True else False -- False :: Bool
> e1e = length [1, 2, 3, 4, 5] -- 5 :: Int
> e1f = (length [1, 2, 3, 4]) > (length "TACOCAT") -- False :: Bool

2. What is the type of `w`?

> x' = 5
> y = x' + 5
> w = y * 10

Answer: `Num a => a`

3. What is the type of `z`?

> x'' = 5
> y' = x'' + 5
> z y' = y' * 10

Answer: `Num a => a -> a`

4. What is the type of `f'`?

> x''' = 5
> y'' = x''' + 5
> f' = 4 / y''

Answer: `Fractional a => a`

5. What is the type of `f''`?

> x'''' = "Julie"
> y''' = " <3 "
> z' = "Haskell"
> f'' = x'''' ++ y''' ++ z'

Answer: [Char]

---

Does it compile? Figure out which expression (if any) doesn't compile, and fix it.

1. (corrected from `wahoo = bigNum $ 10`)

> bigNum = (^) 5 $ 10
> wahoo = bigNum

2. (no compiler error predicted, and none occurred)

> x2 = print
> y2 = print "woohoo!"
> z2 = x2 "hello world"

3. (corrected from `b = 5`)

> a = (+)
> b = a
> c = b 10
> d = c 200

4. (corrected from no `c'` definition)

> a' = 12 + b'
> b' = 10000 * c'
> c' = 1

---

Type variable or specific type constructor?

You will be shown a type declaration, and you should categorize each type as one of:

* fully polymorphic type variable
* constrained polymorphic type variable
* concrete type constructor

1. f :: Num a => a -> b -> Int -> Int
a: constrained
b: parametric
Int: concrete

2. f :: zed -> Zed -> Blah
zed: parametric
Zed: concrete
Blah: concrete

3. f :: Enum b => a -> b -> C
a: parametric
b: constrained (to Enum)
C: concrete

4. f :: f -> g -> C
f: parametric
g: parametric
C: concrete

---

Write a type signature

1.

> functionH :: [a] -> a
> functionH (x:_) = x

2.

> functionC :: Ord a => a -> a -> Bool
> functionC x y =
>   if (x > y) then True else False

3.

> functionS :: (a, b) -> b
> functionS (x, y) = y

---

Given a type, write the function

1. Only one definition that doesn't loop infinitely.

> i :: a -> a
> i = id

2. There is only one version that works.

> c2 :: a -> b -> a
> c2 = const

3. Given alpha equivalence are c3 and c2 (see above) the same thing? (Yes.)

> c3 :: b -> a -> b
> c3 = c2

4. One one version that works.

> c4 :: a -> b -> b
> c4 = const id

5. Multiple possibilities, including two previously seen.

> r :: [a] -> [a]
> r [] = []
> r (x:xs) = [x]

6. Only one version that will typecheck.

> co :: (b -> c) -> (a -> b) -> a -> c
> co = (.)

7. One version will typecheck.

> a3 :: (a -> c) -> a -> a
> a3 = const id

8. One version will typecheck.

> a4 :: (a -> b) -> a -> b
> a4 = id

---

Fix it

1. (See 05-sing.hs)
2. Now that it’s fixed, make a minor change and make it sing the other song. If you’re lucky, you’ll end up with both songs stuck in your head! (done)
3. (See 06-arith3broken.hs)

---

Type-Kwon-Do

Using the provided definitions (some of which are implemented only using `undefined`), fill in the missing code (last line).

1.

> f1 :: Int -> String
> f1 = undefined
> g1 :: String -> Char
> g1 = undefined
> h1 :: Int -> Char
> h1 = g1 . f1

2.

> data A
> data B
> data C
> q2 :: A -> B
> q2 = undefined
> w2 :: B -> C
> w2 = undefined
> e2 :: A -> C
> e2 = w2 . q2

3.

> data X
> data Y
> data Z
> xz :: X -> Z
> xz = undefined
> yz :: Y -> Z
> yz = undefined
> xform :: (X, Y) -> (Z, Z)
> xform (x, y) = (xz x, yz y)

4.

> munge :: (x -> y)
>       -> (y -> (w, z))
>       -> x
>       -> w
> munge x2y y2wz = fst . y2wz . x2y
