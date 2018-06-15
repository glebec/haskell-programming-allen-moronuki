> {-# OPTIONS_GHC -W #-}
> {-# LANGUAGE FlexibleInstances #-}

> module Chapter11Exercises where
> import Data.Int
> import Data.Char
> import Data.List (elemIndex, find, sort, sortOn, group, intersperse)
> import Data.Maybe (fromMaybe)

\section{11.5 Data constructors and values}

Exercises: Dog Types

> data PugType = PugData
> data HuskyType a = HuskyData
> data DogueDeBordeaux doge = DogueDeBordeaux doge
> data Doggies a = Husky a | Mastiff a deriving (Eq, Show)

1. Is `Doggies` a type constructor or a data constructor? A: type.
2. What is the kind of Doggies? A: `* -> *`
3. What is the kind of Doggies String? A: `*`
4. What is the type of Husky 10? A: `Num a => Doggies a`
5. What is the type of `Husky (10 :: Integer)`? A: `Doggies Integer`
6. What is the type of `Mastiff "Scooby Doo"`? A: `Doggies String`
7. Is `DogueDeBordeaux` a type constructor or a data constructor? A: both
8. What is the type of `DogueDeBordeaux`? A: `doge -> DogueDeBordeaux doge`
9. What is the type of `DogueDeBordeaux "doggie!"`? A: `DogueDeBordeaux String`


\section{11.6 What's a type and what's data?}

> data Price = Price Integer deriving (Eq, Show)
> data Manufacturer = Mini
>                   | Mazda
>                   | Tata deriving (Eq, Show)
> data Airline = PapuAir
>              | CatapultsR'Us
>              | TakeYourChancesUnited deriving (Eq, Show)
> data Vehicle = Car Manufacturer Price
>              | Plane Airline Size deriving (Eq, Show)
> data Size = Jumbo | Regional | Recreational deriving (Show, Eq) -- exercise

> myCar    = Car Mini (Price 14000)
> urCar    = Car Mazda (Price 20000)
> clownCar = Car Tata (Price 7000)
> doge     = Plane PapuAir Recreational

1. What is the type of myCar? A: `Vehicle`
2. Given the following, define the functions:

> isCar :: Vehicle -> Bool
> isCar (Car _ _) = True
> isCar _         = False

> isPlane :: Vehicle -> Bool
> isPlane (Plane _ _) = True
> isPlane _           = False

> areCars :: [Vehicle] -> [Bool]
> areCars = map isCar

3. Likewise:

> getManu :: Vehicle -> Manufacturer
> getManu (Car m _) = m

4. Given that we’re returning the `Manufacturer`, what will happen if you use this on `Plane` data? A: we'd get a runtime error as this is a partial function, and ought to be defined with a different type (return a Maybe).
5. All right. Let’s say you’ve decided to add the size of the plane as an argument to the `Plane` constructor. Add that to your datatypes in the appropriate places and change your data and functions appropriately.


\section{11.8 What makes these datatypes algebraic?}

Exercise: Cardinality

1. `data PugType = PugData` -- 1
2. `Airline` -- 3
3. `Int16` --- 65536
4. `Int` has finite cardinality, `Integer` infinite.
5. 2^8

Exercises: For Example

> data Example = MakeExample deriving Show

1. What is the type of data constructor `MakeExample`? What happens when you request the type of `Example`? A: `Example`, and the REPL complains that `Example` isn't a constructor.
2. Can you determine what typeclass instances are defined for the `Example` type using :i in GHCi? A: yes
3. Try making a new datatype like `Example` but with a single type argument added to `MakeExample`, such as `Int`. What has changed when you query `MakeExample` with :type in GHCi?

> data Ex2 = MakeE2 Int deriving Show

A: it becomes `Int -> Ex2`


\section{11.9 newtype}

Exercises: Logic Goats

1. Reusing the `TooMany` typeclass, write an instance of the typeclass for the type `(Int, String)`. (Will require `FlexibleInstances`).

> class TooMany a where tooMany :: a -> Bool

> instance TooMany (Int, String) where tooMany (a, _) = a > 42

2. Make another `TooMany` instance for `(Int, Int)`. Sum the values together under the assumption this is a count of goats from two fields.

> instance TooMany (Int, Int) where tooMany (a, b) = a + b > 42

3. Make another `TooMany` instance, this time for `(Num a, TooMany a) => (a, a)`. This can mean whatever you want, such as summing the two numbers together.

> instance TooMany Integer where tooMany a = a > 99
> instance (Num a, TooMany a) => TooMany (a, a) where
>     tooMany (a, b) = tooMany (a + b)


\section{11.10 Sum types}

Exercises: Pity the Bool

1. Given:

> data BigSmall = Big Bool | Small Bool deriving (Eq, Show)

What is the cardinality of this datatype? A: 4.

2. Given:

> data NumberOrBool = Numba Int8 | BoolyBool Bool deriving (Eq, Show)

What is the cardinality? A: 256 + 2 = 258.


\section{11.12 Normal Form}

Exercises: How Does Your Garden Grow?

1. Given:

> data FlowerType = TheGardenia
>                 | TheDaisy
>                 | TheRose
>                 | TheLilac
>                 deriving Show
> type Gardener = String
> -- data Garden = Garden Gardener FlowerType deriving Show

What is the sum of products normal form of `Garden`?

> data Garden = Gardenia Gardener
>             | Daisy    Gardener
>             | Rose     Gardener
>             | Lilac    Gardener
>             deriving Show


\section{11.13 Constructing and deconstructing values}

Exercise: Programmers

Write a function that generates all possible values of Programmer.

> data OperatingSystem =
>      GnuPlusLinux
>    | OpenBSDPlusNevermindJustBSDStill
>    | Mac
>    | Windows
>    deriving (Eq, Show)

> data ProgLang =
>      Haskell
>    | Agda
>    | Idris
>    | PureScript
>    deriving (Eq, Show)

> data Programmer =
>   Programmer { os   :: OperatingSystem
>              , lang :: ProgLang }
>   deriving (Eq, Show)

> allOperatingSystems :: [OperatingSystem]
> allOperatingSystems =
>   [ GnuPlusLinux
>   , OpenBSDPlusNevermindJustBSDStill
>   , Mac
>   , Windows
>   ]

> allLanguages :: [ProgLang]
> allLanguages = [Haskell, Agda, Idris, PureScript]

> allProgrammers :: [Programmer]
> allProgrammers = [ Programmer {os = o, lang = l} -- 16 (= 4 * 4)
>   | o <- allOperatingSystems, l <- allLanguages ]


\section{11.14 Function type is exponential}

Exercises: The Quad

Determine how many unique inhabitants each type has.

> data Quad = One
>           | Two
>           | Three
>           | Four
>           deriving (Eq, Show)
> -- 1. how many different forms can this take?
> eQuad :: Either Quad Quad
> eQuad = undefined

A: 4 + 4 = 8

> prodQuad :: (Quad, Quad) -- 2
> prodQuad = undefined

A: 4 * 4 = 16

> funcQuad :: Quad -> Quad -- 3
> funcQuad = undefined

A: 4 ^ 4 = 256

> prodTBool :: (Bool, Bool, Bool) -- 4
> prodTBool = undefined

A: 2 * 2 * 2 = 8

> gTwo :: Bool -> Bool -> Bool -- 5
> gTwo = undefined

A: 2 ^ 2 ^ 2 = 16

> fTwo :: Bool -> Quad -> Quad -- 6
> fTwo = undefined

A: 2 ^ 4 ^ 4 = 65,536


\section{11.17 Binary Tree}

> data BinaryTree a = Leaf
>                   | Node (BinaryTree a) a (BinaryTree a)
>                   deriving (Eq, Ord, Show)

> insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
> insert' b Leaf = Node Leaf b Leaf
> insert' b (Node left a right)
>   | b == a = Node left a right
>   | b < a  = Node (insert' b left) a right
>   | b > a  = Node left a (insert' b right)

Write map for BinaryTree.

> mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
> mapTree _ Leaf = Leaf
> mapTree f (Node left a right) =
>   Node (mapTree f left) (f a) (mapTree f right)

> testTree', mapExpected :: BinaryTree Integer
> testTree'   = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)
> mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

> -- acceptance test for mapTree
> mapOkay =
>   if mapTree (+1) testTree' == mapExpected
>   then print "yup okay!" -- yup indeed.
>   else error "test failed!"

Convert binary trees to lists.

> preorder :: BinaryTree a -> [a]
> preorder Leaf = []
> preorder (Node left a right) = a : (preorder left) ++ (preorder right)

> inorder :: BinaryTree a -> [a]
> inorder Leaf = []
> inorder (Node left a right) = (inorder left) ++ (a : (inorder right))

> postorder :: BinaryTree a -> [a]
> postorder Leaf = []
> postorder (Node left a right) = (postorder left) ++ (postorder right) ++ [a]

> testTree :: BinaryTree Integer
> testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

> testPreorder :: IO ()
> testPreorder = if preorder testTree == [2, 1, 3]
>                then putStrLn "Preorder fine!"
>                else putStrLn "Bad news bears."

> testInorder :: IO ()
> testInorder = if inorder testTree == [1, 2, 3]
>               then putStrLn "Inorder fine!"
>               else putStrLn "Bad news bears."

> testPostorder :: IO ()
> testPostorder = if postorder testTree == [1, 3, 2]
>                 then putStrLn "Postorder fine!"
>                 else putStrLn "postorder failed check"

> main :: IO ()
> main = do
>   testPreorder
>   testInorder
>   testPostorder

Write foldr for BinaryTree

> -- any traversal order is fine
> foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
> foldTree f b t = foldr f b (inorder t)


\section{11.18 Chapter Exercises}

Multiple Choice

1. Given a `data Weekday = Mon | Tue | ...` type, we can say a) Weekday is a type with five data constructors.
2. With the same datatype definition in mind, what is the type of `f` in `f Friday = "Miller Time"`? c) `Weekday -> String`
3. Types defined with the `data` keyword b) must begin with a capital letter.
4. The function `g xs = xs !! (length xs - 1)` c) delivers the final element of xs.

Ciphers

(See 02-cipher.hs)

As-patterns

1. This should return `True` iff all the values in the first list appear in the second, in correct order (but not necessarily contiguous).

> isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
> isSubseqOf [] _  = True
> isSubseqOf _  [] = False
> isSubseqOf needles@(n:ns) (h:hs)
>   | n == h    = isSubseqOf ns hs
>   | otherwise = isSubseqOf needles hs

2. Split a sentence into words, then tuple each word with the capitalized form of each.

> capitalizeWords :: String -> [(String, String)]
> capitalizeWords = map (\w@(l:ls) -> (w, toUpper l : ls)) . words

Language exercises

1. Write a function that capitalizes a word.

> capitalizeWord :: String -> String
> capitalizeWord "" = ""
> capitalizeWord (x:xs) = toUpper x : xs

2. Write a function that capitalizes sentences in a paragraph. Recognize when a new sentence has begun by checking for periods. Reuse the `capitalizeWord` function.

> capitalizeParagraph :: String -> String
> capitalizeParagraph "" = ""
> capitalizeParagraph p = capitalizeWord s1 ++ endOfS1 ++ capitalizeParagraph remainder
>    where isElem    = elem :: Char -> String -> Bool -- to solve an ambiguous type
>          s1        = takeWhile (/= '.') p
>          endOfS1   = takeWhile (`isElem` ". ") (drop (length s1) p)
>          remainder = drop (length s1 + length endOfS1) p
> -- not totally enamored with this solution…

Phone exercise

Phone keypad: 1, 2 ABC, 3 DEF, 4 GHI, 5 JKL, 6 MNO, 7 PQRS, 8 TUV, 9 WXYZ, * ^, 0 +-, # .

- star is capitalization
- 0 for space
- keep pressing to type number literal
- wraps around

1. Create a data structure that captures the phone layout above. The data structure should be able to express enough of how the layout works that you can use it to dictate the behavior of the functions in the following exercises.

> data DaPhone = DaPhone [(Char, String)]
> daPhone = DaPhone
>   [ ('1', "")
>   , ('2', "abc2")
>   , ('3', "def3")
>   , ('4', "ghi4")
>   , ('5', "jkl5")
>   , ('6', "mno6")
>   , ('7', "pqrs7")
>   , ('8', "tuv8")
>   , ('9', "wxyz9")
>   , ('*', "^*")
>   , ('0', " +-0")
>   , ('#', ".#") ]

> getPresses :: Char -> String -> Int
> getPresses c cs = case c `elemIndex` cs of
>   Just i  -> i + 1
>   Nothing -> 0

> findOnPhone :: DaPhone -> Char -> (Digit, Presses)
> findOnPhone (DaPhone buttons) char = case find correct buttons of
>   Just (d, cs) -> (d, getPresses c cs)
>   Nothing      -> ('?', 0)
>   where correct (_, cs) = c `elem` cs
>         c = toLower char

2. Convert the following conversations into the keypresses required to express them.

> convo :: [String]
> convo =
>   ["Wanna play 20 questions",
>    "Ya",
>    "U 1st haha",
>    "Lol ok. Have u ever tasted alcohol",
>    "Lol ya",
>    "Wow ur cool haha. Ur turn",
>    "Ok. Do u think I am pretty Lol",
>    "Lol ya",
>    "Just making sure rofl ur turn"]

> -- validButtons = "1234567890*#"
> type Digit = Char

> -- Valid presses: 1 and up
> type Presses = Int

> reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
> reverseTaps phone c
>   | isUpper c = [('*', 1), findOnPhone phone c]
>   | otherwise = [findOnPhone phone c]

> cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
> cellPhonesDead phone s = concat $ map (reverseTaps phone) s

3. How many times do digits need to be pressed for each message?

> fingerTaps :: [(Digit, Presses)] -> Presses
> fingerTaps = sum . map snd

> a3 = map fingerTaps (map (cellPhonesDead daPhone) convo) -- [50,5,16,71,15,49,58,15,60]

4. What was the most popular letter for each message? What was its cost? You’ll want to combine `reverseTaps` and `fingerTaps` to figure out what it cost in taps. `reverseTaps` is a list because you need to press a different button in order to get capitals.

> mostPopular :: Ord c => [c] -> c
> mostPopular = head . head . sortOn (negate . length) . group . sort

> mostPopularLetter :: String -> Char
> mostPopularLetter = mostPopular

> mostPopularWord :: String -> String
> mostPopularWord = mostPopular . words

> letterCost :: DaPhone -> Char -> String -> Presses
> letterCost p c s = numberTimes * (fingerTaps . reverseTaps p $ c)
>   where numberTimes = length $ filter (== c) s

5. What was the most popular letter overall? What was the most popular word?

> coolestLtr :: [String] -> Char
> coolestLtr = mostPopularLetter . concat . intersperse "\n"

> a5i = coolestLtr convo -- ' '

> coolestWord :: [String] -> String
> coolestWord = mostPopularWord . concat . intersperse "\n"

> a5ii = coolestWord convo -- "u"

Opinion: what a slog that was.

Hutton's Razor

1. Your first task is to write the “eval” function which reduces an expression to a final sum.

> data Expr
>   = Lit Integer
>   | Add Expr Expr
>   deriving (Eq, Show)

> eval :: Expr -> Integer
> eval (Lit n) = n
> eval (Add e1 e2) = eval e1 + eval e2

2. Write a printer.

> printExpr :: Expr -> String
> printExpr (Lit n) = show n
> printExpr (Add e1 e2) = printExpr e1 ++ " + " ++ printExpr e2

Opinion: that was more fun.
