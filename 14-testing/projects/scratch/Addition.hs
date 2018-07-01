module Addition where

import Test.Hspec
import Test.QuickCheck

sayHello :: IO ()
sayHello = putStrLn "hello!"

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
    where go n d count
           | n < d = (count, n)
           | otherwise = go (n - d) d (count + 1)

main :: IO ()
main = hspec $ describe "Addition" $ do
    describe "examples" $ do
        it "1 + 1 is greater than 1" $
            (1 + 1) > 1 `shouldBe` True
        it "2 + 2 equals 4" $
            2 + 2 `shouldBe` 4
        it "x + 1 is always > x" $
            property $ \x -> x + 1 > (x :: Int)
    describe "dividedBy" $ do
        it "15 divided by 3 is 5" $
            15 `dividedBy` 3 `shouldBe` (5, 0)
        it "22 divided by 5 is 4 rem 2" $
            22 `dividedBy` 5 `shouldBe` (4, 2)

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater
