module WordNumberTest where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Property (expectFailure)
import WordNumber (digitToWord, digits, wordNumber)
import UsingQC
import Data.List (sort)
import Data.Char (toUpper)

-- UsingQC

-- 1.

halfIdentity :: Fractional a => a -> a
halfIdentity = (*2) . half

prop_halfIdentity :: (Eq a, Fractional a) => a -> Bool
prop_halfIdentity x = halfIdentity x == x

-- 2.

prop_sortedIsOrdered :: (Ord a) => [a] -> Bool
prop_sortedIsOrdered = listOrdered . sort

-- 3.

prop_plusAssociative :: (Num a, Eq a) => a -> a -> a -> Bool
prop_plusAssociative x y z =
    x + (y + z) == (x + y) + z

prop_plusCommutative :: (Num a, Eq a) => a -> a -> Bool
prop_plusCommutative x y =
    x + y == y + x

-- 4.

prop_multAssociative :: (Num a, Eq a) => a -> a -> a -> Bool
prop_multAssociative x y z =
    x * (y * z) == (x * y) * z

prop_multCommutative :: (Num a, Eq a) => a -> a -> Bool
prop_multCommutative x y =
    x * y == y * x

-- 5.

-- ah, this fails for division by zero.
prop_quotRem :: (Integral a, Eq a) => a -> a -> Bool
prop_quotRem x y = (x `quot` y) * y + (x `rem` y) == x

prop_divMod :: (Integral a, Eq a) => a -> a -> Bool
prop_divMod x y = (x `div` y) * y + (x `mod` y) == x

-- have to make a generator for nonzero values
genNonZeroPair :: (Eq a, Num a, Arbitrary a) => Gen (a, a)
genNonZeroPair = do
    a <- arbitrary :: (Eq a, Num a, Arbitrary a) => Gen (NonZero a)
    b <- arbitrary :: (Eq a, Num a, Arbitrary a) => Gen (NonZero a)
    return (getNonZero a, getNonZero b)

-- now we can make props that test nonzero vals
prop_quotRemNonZero :: Property
prop_quotRemNonZero =
    forAll (genNonZeroPair :: Gen (Int, Int)) $ uncurry prop_quotRem

prop_divModNonZero :: Property
prop_divModNonZero =
    forAll (genNonZeroPair :: Gen (Int, Int)) $ uncurry prop_divMod

-- 6.

prop_powerAssociative :: (Integral a, Eq a) => a -> a -> a -> Bool
prop_powerAssociative x y z =
    x ^ (y ^ z) == (x ^ y) ^ z

prop_powerCommutative :: (Integral a, Eq a) => a -> a -> Bool
prop_powerCommutative x y =
    x ^ y == y ^ x

-- 7.

prop_reverseIdentity :: Eq a => [a] -> Bool
prop_reverseIdentity xs = (reverse . reverse $ xs) == xs

-- 8.

-- I am not sure what is intended here. A good check would generate random
-- functions, but generated values need a `show` instance for QuickCheck.

prop_apply :: Eq a => a -> Bool
prop_apply a = id a == (id $ a)

-- 9.

-- nope, backwards!
prop_foldrAppend :: Eq a => [a] -> [a] -> Bool
prop_foldrAppend xs tx = foldr (:) xs tx == xs ++ tx

prop_foldrConcat :: Eq a => [[a]] -> Bool
prop_foldrConcat xs = foldr (++) [] xs == concat xs

-- 10.

prop_takeN :: Int -> [a] -> Bool
prop_takeN n xs = length (take n xs) == n

-- 11.

prop_readShowIdentity :: (Show a, Read a, Eq a) => a -> Bool
prop_readShowIdentity x = (read . show) x == x

-- Failure

-- 1.

square :: Num a => a -> a
square x = x * x

prop_squareIdentity :: (Eq a, Floating a) => a -> Bool
prop_squareIdentity x = (square . sqrt) x == x

-- Idempotence

twice :: (a -> a) -> (a -> a)
twice f = f . f

fourTimes :: (a -> a) -> (a -> a)
fourTimes = twice . twice

-- 1.

capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (x:xs) = toUpper x : xs

prop_capitalizeIdempotent :: String -> Bool
prop_capitalizeIdempotent x =
    (capitalizeWord x == twice capitalizeWord x) &&
    (capitalizeWord x == fourTimes capitalizeWord x)

-- 2.

prop_sortIdempotent :: Ord a => [a] -> Bool
prop_sortIdempotent x =
    (sort x == twice sort x) &&
    (sort x == fourTimes sort x)

-- Making Generators

-- 1. (equal probabilities)

data Fool =
      Fulse
    | Frue
    deriving (Eq, Show)

chooseFool :: Gen Fool
chooseFool = elements [Fulse, Frue]

-- 2. (use 2/3 chance Frue, 1/3 Fulse)

likelyFrue :: Gen Fool
likelyFrue = frequency [(2, return Frue), (1, return Fulse)]
-- likelyFrue = elements [Frue, Frue, Fulse]

-- spec

main :: IO ()
main = hspec $ do
    describe "WordNumber" $ do
        describe "digitToWord" $ do
            it "returns zero for 0" $
                digitToWord 0 `shouldBe` "zero"
            it "returns one for 1" $
                digitToWord 1 `shouldBe` "one"
        describe "digits" $ do
            it "returns [1] for 1" $
                digits 1 `shouldBe` [1]
            it "returns [1, 0, 0] for 100" $
                digits 100 `shouldBe` [1, 0, 0]
        describe "wordNumber" $ do
            it "one-zero-zero given 100" $
                wordNumber 100 `shouldBe` "one-zero-zero"
            it "nine-zero-zero-one for 9001" $
                wordNumber 9001 `shouldBe` "nine-zero-zero-one"
    describe "UsingQC" $ do
        describe "half" $ do
            it "is the inverse of doubling (Float)" $
                property (prop_halfIdentity :: Float -> Bool)
            it "is the inverse of doubling (Double)" $
                property (prop_halfIdentity :: Double -> Bool)
        describe "sort" $ do
            it "puts Ord values in order (Ordering)" $
                property (prop_sortedIsOrdered :: [Ordering] -> Bool)
            it "puts Ord values in order (Bool)" $
                property (prop_sortedIsOrdered :: [Bool] -> Bool)
            it "puts Ord values in order (Int)" $
                property (prop_sortedIsOrdered :: [Int] -> Bool)
            it "puts Ord values in order (String)" $
                property (prop_sortedIsOrdered :: [String] -> Bool)
        describe "(+)" $ do
            it "is associative (Int)" $
                property (prop_plusAssociative :: Int -> Int -> Int -> Bool)
            it "is associative (Integer)" $
                property (prop_plusAssociative :: Integer -> Integer -> Integer -> Bool)
            it "is commutative (Int)" $
                property (prop_plusCommutative :: Int -> Int -> Bool)
            it "is commutative (Integer)" $
                property (prop_plusCommutative :: Integer -> Integer -> Bool)
        describe "(*)" $ do
            it "is associative (Int)" $
                property (prop_multAssociative :: Int -> Int -> Int -> Bool)
            it "is associative (Integer)" $
                property (prop_multAssociative :: Integer -> Integer -> Integer -> Bool)
            it "is commutative (Int)" $
                property (prop_multCommutative :: Int -> Int -> Bool)
            it "is commutative (Integer)" $
                property (prop_multCommutative :: Integer -> Integer -> Bool)
        describe "`quot` and `rem`" $
            it "are complementary (nonzero Int)" $
                property prop_quotRemNonZero
        describe "`div` and `rem`" $
            it "are complementary (nonzero Int)" $
                property prop_divModNonZero
        describe "(^)" $ do
            it "is not associative (Int)" $
                expectFailure (prop_powerAssociative :: Int -> Int -> Int -> Bool)
            it "is not commutative (Int)" $
                expectFailure (prop_powerCommutative :: Int -> Int -> Bool)
        describe "reverse . reverse" $ do
            it "is the list identity (Int)" $
                property (prop_reverseIdentity :: [Int] -> Bool)
            it "is the list identity (String)" $
                property (prop_reverseIdentity :: [String] -> Bool)
        describe "($)" $
            it "is function application" $
                property (prop_apply :: Int -> Bool)
        describe "`foldr (:)`" $
            it "is not the same as (++)" $
                expectFailure (prop_foldrAppend :: [Int] -> [Int] -> Bool)
        describe "`foldr (:) []`" $
            it "is the same as `concat`" $
                property (prop_foldrConcat :: [[Int]] -> Bool)
        describe "`the length of `take`ing `n` items" $
            it "is not always `n`" $
                expectFailure (prop_takeN :: Int -> String -> Bool)
        describe "`read . show`" $
            it "is an identity" $
                property (prop_readShowIdentity :: String -> Bool)
    describe "failure" $
        describe "squaring the square root" $
            it "fails with IEEE 754 types (Double)" $
                expectFailure (prop_squareIdentity :: Double -> Bool)
    describe "idempotence" $ do
        describe "capitalizeWord" $
            it "is idempotent" $
                property prop_capitalizeIdempotent
        describe "sort" $
            it "is idempotent" $
                property (prop_sortIdempotent :: [Int] -> Bool) .&&.
                property (prop_sortIdempotent :: [String] -> Bool)
