> {-# OPTIONS_GHC -W #-}

> module Chapter6Exercises where
> import Data.List (sort)

\section{6.5}

Write the Eq instance for the datatype provided.

1. It’s not a typo, we’re just being cute with the name.

> data TisAnInteger =
>     TisAn Integer

> instance Eq TisAnInteger where
>     TisAn a == TisAn b =
>           a == b

2.

> data TwoIntegers =
>     Two Integer Integer

> instance Eq TwoIntegers where
>     Two a b == Two a' b' =
>           a == a' &&
>           b == b'

3.

> data StringOrInt =
>     TisAnInt Int | TisAString String

> instance Eq StringOrInt where
>     TisAnInt i   == TisAnInt i'   = i == i'
>     TisAString s == TisAString s' = s == s'
>     TisAnInt _   == TisAString _  = False -- compiler yells if don't use `_`
>     TisAString _ == TisAnInt _    = False -- had to include this line for total pattern matching.

4.

> data Pair a =
>     Pair a a

> instance Eq t => Eq (Pair t) where
>     Pair a b == Pair a' b' = a == a' && b == b'

5.

> data Tuple a b =
>     Tuple a b

> instance (Eq a, Eq b) => Eq (Tuple a b) where
>     Tuple a b == Tuple a' b' = a == a' && b == b'

6.

> data Which a = ThisOne a | ThatOne a

> instance Eq a => Eq (Which a) where
>     ThisOne a == ThisOne a' = a == a'
>     ThatOne a == ThatOne a' = a == a'
>     ThisOne _ == ThatOne _  = False
>     ThatOne _ == ThisOne _  = False

7.

> data EitherOr a b = Hello a | Goodbye b

> instance (Eq a, Eq b) => Eq (EitherOr a b) where
>     Hello a   == Hello a'   = a == a'
>     Goodbye a == Goodbye a' = a == a'
>     Hello _   == Goodbye _  = False
>     Goodbye _ == Hello _    = False


\section{6.6}

Tuple Experiment

Look at the types given for `quotRem` and `divMod`. What do you think those functions do? Test your hy- potheses by playing with them in the REPL.


\section{6.8}

Take a look at the following code examples and try to decide if they will work, what result they will return if they do, and why or why not.

1. Returns 5. Had to move second argument to the same line…

> res1 = max (length [1, 2, 3]) (length [8, 9, 10, 11, 12])

2. Yes, will return LT.

> res2 = compare (3 * 4) (3 * 5)

3. Will throw an error. Mixing types, `compare` operates on same type.

> -- compare "Julie" True

4. Returns False.

> res4 = (5 + 3) > (3 + 6)


\section{6.14}

Chapter Exercises

---

Multiple choice

1. The Eq class…
c) makes equality tests possible

2. The typeclass Ord…
a) allows any two values to be compared*
*(assuming this implies two values of a type which has an instance of Ord.)

3. Suppose the typeclass Ord has an operator >. What is the type of >?
a) Ord a => a -> a -> Bool

4. In x = divMod 16 12
b) the value of 𝑥 is undecidable

5. The typeclass Integral includes
a) Int and Integer numbers

---

Does it typecheck?

Examine the following code and decide whether it will typecheck. If it won't, see if you can fix the error.

1.

> data Person = Person Bool deriving Show
> printPerson :: Person -> IO ()
> printPerson person = putStrLn (show person)

No instance for showing a person. Added `deriving Show` to fix.

2.

> data Mood = Blah | Woot deriving (Show, Eq)
> settleDown x = if x == Woot then Blah else x

No instance for Eq. Added Eq to deriving list.

3. for the fixed `settleDown`:

a) What values are acceptable inputs to that function? A: Moods.
b) What will happen if you try to run settleDown 9? Why? A: typechecker will complain because x needs to be a Mood to check equality with Woot.
c) What will happen if you try to run Blah > Woot? Why? A: no instance for Ord Mood.

4.

> type Subject = String
> type Verb = String
> type Object = String

> data Sentence = Sentence Subject Verb Object deriving (Eq, Show)

> s1 = Sentence "dogs" "drool"
> s2 = Sentence "Julie" "loves" "dogs"

Interesting – does typecheck, which I didn't expect. `s1` ends up being a function waiting for the final string argument to become a full Sentence.

---

Given the following datatype definitions:

> data Rocks = Rocks String deriving (Eq, Show, Ord) -- (Eq, Show)
> data Yeah = Yeah Bool deriving (Eq, Show, Ord) -- (Eq, Show)
> data Papu = Papu Rocks Yeah deriving (Eq, Show, Ord) -- (Eq, Show)

Which of the following will typecheck? For the ones that don’t, why don’t they? (Listing original in comments, fixed is active).

> phew = Papu (Rocks "chases") (Yeah True) -- phew = Papu "chases" True

> truth = Papu (Rocks "chomskydoz") (Yeah True)

> equalityForall :: Papu -> Papu -> Bool
> equalityForall p p' = p == p' -- 3

> comparePapus :: Papu -> Papu -> Bool
> comparePapus p p' = p > p' -- had to add Ord to all datatypes -- 4

---

Match the types: for each pair of types, can you substitute the second type for the first?

1.

> i :: Num a => a
> -- i :: a
> i = 1

No – a parametric type like `a` has no instance of Num (in fact it has no instance of anything).

2.

> f2 :: Float
> -- f2 :: Num a => a
> f2 = 1.0

No – `Num` is too permissive, 1.0 is at most Fractional.

3.

> -- f3 :: Float
> f3 :: Fractional a => a
> f3 = 1.0

Yes.

4. Hint for the following: type :info RealFrac in your REPL.

> -- f4 :: Float
> f4 :: RealFrac a => a
> f4 = 1.0

Yes? Yes.

5.

> -- freud :: a -> a
> freud :: Ord a => a -> a
> freud x = x

Yes, though Ord is more restrictive.

6.

> -- freud' :: a -> a
> freud' :: Int -> Int
> freud' x = x

Yes.

7.

> myX = 1 :: Int
> sigmund :: Int -> Int
> -- sigmund :: a -> a
> sigmund x = myX

No, sigmund cannot take any `a` because it returns myX which is Int.

8.

> myX8 = 1 :: Int
> sigmund' :: Int -> Int
> -- sigmund' :: Num a => a -> a
> sigmund' x = myX8

No, sigmund' could not take any arbitrary a as Num because it returns an a that is always Int.

9. You’ll need to import sort from Data.List.

> -- jung :: Ord a => [a] -> a
> jung :: [Int] -> Int
> jung xs = head (sort xs)

Sure, though [Int] is less useful than Ord a => [a].

10.

> -- young :: Ord a => [a] -> a
> young :: [Char] -> Char
> young xs = head (sort xs)

Yes, same idea as last problem.

11.

> mySort :: [Char] -> [Char]
> mySort = sort

> signifier :: [Char] -> Char
> -- signifier :: Ord a => [a] -> a
> signifier xs = head (mySort xs)

No, because `mySort` requires [Char]. Ord a => a is too general.

---

Type-Kwon-Do

Fill in terms (code) that fit the type. You’ll probably need to use stuff from Prelude.

1.

> chk :: Eq b => (a -> b) -> a -> b -> Bool
> chk fn a b = (fn a) == b

2.

Hint: use some arithmetic operation to combine values of type 'b'. Pick one.

> arith :: Num b => (a -> b) -> Integer -> a -> b
> arith fn int a = (fn a) + (fromInteger int)
