module Ex27_14 where

-- Q1, see StrictList.hs

-- Q2: What will :spring output?

s1 = let x = 1 in x -- 1 (actual: _)

s2 = let x = ['1'] in x -- _ (correct)

s3 = let x = [1] in x -- _ (correct)

s4 = let x = 1 :: Int in x -- 1 (actual: _)

s5 = let f = \x -> x
         x = f 1
     in  x -- 1 (actual: _)

s6 = let f :: Int -> Int
         f = \x -> x
         x = f 1
     in  x -- 1 (actual: _)

-- GHC 8.6.? seems to thunk everything…

-- Q3: Will printing this expression result in bottom?

b1 = snd (undefined, 1) -- no

b2 = let x = undefined
         y = x `seq` 1
     in  snd (x, y) -- yes

b3 = length $ [1..5] ++ undefined -- yes

b4 = length $ [1..5] ++ [undefined] -- no

b5 = const 1 undefined -- no

b6 = const 1 (undefined `seq` 1) -- no

b7 = const undefined 1 -- yes

-- Q4: Make the expression bottom, only using `seq` or bang pattern(s)

x = undefined
y = x `seq` "blah"

main :: IO ()
main = print (snd (x, y))
