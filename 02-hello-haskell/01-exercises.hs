module Chapter2Exercises where

-- 2.5 Exercises

-- 2.5:1 Given the following lines of code as they might appear in a source file, how would you change them to use them directly in the REPL?

half x = x / 2
square x = x * x

-- These seem to work as-is in the REPL. Prepending `let` or not doesn't seem to make any difference. Perhaps because the book assumes an earlier version of the REPL (pre-8)? Cursory SO search suggests something about `do` notation.

-- Here they are sectioned and point-free, for fun:

halfPF = (/2)
squarePF = (^2)

-- can we assign lambdas to a var? I assume so, let's check. Yep:

halfL = \x -> x/2

-- HLint yells at the above, prefers `(/2)`.

-- 2.5:2 Write one function that can accept one argument and work for all the following expressions. Be sure to name the function.

-- 3.14 * (5 * 5)
-- 3.14 * (10 * 10)
-- 3.14 * (2 * 2)
-- 3.14 * (4 * 4)

circleArea1 r = 3.14 * r^2
circleArea2 = (3.14 *) . (^2)
circleArea3 = (3.14 *) . square

-- 2.5:3 There is a value in Prelude called pi. Rewrite your function to use pi instead of 3.14.

circleArea = (pi *) . square

-- naturally, we get more precision out of `pi`.

-------------------

-- 2.6 Below are some pairs of functions that are alike except for parenthe- sization. Read them carefully and decide if the parentheses change the results of the function. Check your work in GHCi.

-- 1
-- a)
resA = 8 + 7 * 9
-- b)
resB = (8 + 7) * 9
-- yes

-- 2
-- a)
perimeterA x y = (x * 2) + (y * 2)
-- b)
perimeterB x y = x * 2 + y * 2
-- no

-- 3. a)
fA x = x / 2 + 9
-- b)
fB x = x / (2 + 9)
-- yes

-----------------------

-- 2.7 The following code samples are broken and won’t compile. The first two are as you might enter into the REPL; the third is from a source file. Find the mistakes and fix them so that they will.

-- 1 let area x = 3. 14 * (x * x)
area x = 3.14 * (x * x)

-- 2 let double x = b * 2
double x = x * 2

-- 3. x = 7
--     y = 10
--    f = x + y

x = 7
y = 10
f = x + y

-----------------------

-- 2.10 First, determine in your head what the following expressions will return, then validate in the REPL

-- let x = 5 in x -- 5
-- let x = 5 in x * x -- 25
-- let x = 5; y = 6 in x * y -- 30
-- let x = 3; y = 1000 in x + 3 -- 6

r1 = x     where x = 5
r2 = x * x where x = 5
r3 = x * y where x = 5; y = 6
r4 = x + 3 where x = 3; y = 1000

-- Rewrite with where clauses:

-- 1) let x = 3; y = 1000 in x * 3 + y
e1 = x + 3 + y
     where x = 3; y = 1000
-- 2) let y = 10; x = 10 * 5 + y in x * 5
e2 = x * 5
     where x = 10 * 5 + y;
           y = 10
-- 3) let x = 7
--        y = negate x
--        z = y * 10 in z / x + y
e3 = z / x + y
     where x = 7;
           y = negate x;
           z = y * 10

-----------------------

-- 2.11 Chapter Exercises

-- Parenthesization

-- Given what we know about the precedence of (*), (+), and (^), how can we parenthesize the following expressions more explicitly with- out changing their results?

-- 1) 2 + 2 * 3 - 1
a1 = 2 + (2 * 3) - 1
-- 2) (^) 10 $ 1 + 1
a2 = ((^) 10) (1 + 1)
-- 3) 2 ^ 2 * 4 ^ 5 + 1
a3 = ((2 ^ 2) * (4 ^ 5)) + 1

-- Equivalent expressions

-- Which of the following pairs of expressions will return the same result when evaluated?

-- 1) 1 + 1
--    2
-- 2) 10 ^ 2
--    10 + 9 * 10
-- 3) 400 - 37
--    (-) 37 400
-- 4) 100 `div` 3
--    100 / 3
-- 5) 2 * 5 + 18
--    2 * (5 + 18)

-- Answer: 1 and 2 only.

-- More fun with functions

-- Look at this code and rewrite it such that it could be evaluated in the REPL.

-- z = 7
-- x = y ^ 2
-- waxOn = x * 5
-- y = z + 8

-- rewritten:

-- z = 7
-- y = z + 8
-- x = y ^ 2
-- waxOn = x * 5

-- using where:

waxOn = x * 5
        where x = y ^ 2;
                  y = z + 8;
                  z = 7

triple x = x * 3

waxOff = (+ 5) . triple
