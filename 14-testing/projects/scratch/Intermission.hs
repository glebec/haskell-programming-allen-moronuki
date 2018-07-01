module Intermission where

import Test.Hspec

-- cannot handle negative without Eq or Ord
multiply :: (Integral a) => a -> a -> a
multiply factor1 factor2 = go factor1 factor2 0
  where go 0  _  _ = 0
        go _  0  _ = 0
        go 1  f2 r = r + f2 -- unnecessary but more performant…
        go f1 1  r = r + f1
        go f1 f2 r = go f1 (f2 - 1) (r + f1)

testMultiply :: IO ()
testMultiply = hspec $ describe "Intermission" $
    describe "multiply" $ do
        it "1 * 1 is 1" $
            1 `multiply` 1 `shouldBe` 1
        it "3 * 7 is 21" $
            3 `multiply` 7 `shouldBe` 21
        it "0 * 2 is 0" $
            0 `multiply` 2 `shouldBe` 0
