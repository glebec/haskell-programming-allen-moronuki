\begin{code}
module Chapter3Exercises where
\end{code}


3.4
========

1) These lines of code are from a REPL session. Is 𝑦 in scope for 𝑧?
Prelude> let x = 5
Prelude> let y = 7
Prelude> let z = x * y

A: yes.

2) These lines of code are from a REPL session. Is h in scope for g? Go with your gut here.

Prelude> let f = 3
Prelude> let g = 6 * f + h

…no. Assuming this is it, there wasn't an `h` declaration earlier.

3) This code sample is from a source  le. Is everything we need to execute area in scope?

area d = pi * (r * r)
r = d / 2

I don't think so. Checking… nope. `d` not in scope.

4) This code is also from a source file. Now are 𝑟 and 𝑑 in scope for area?

> area d = pi * (r * r)
>   where r = d / 2

-- Yes.


3.5
===========

Read the syntax of the following functions and decide whether it will compile.

1) ++ [1, 2, 3] [4, 5, 6]
No: ++ is not in prefix form.

2) '<3' ++ ' Haskell'
No: single quotes are used for characters, not strings.

3) concat ["<3", " Haskell"]
Yes


3.8
============

1) For the following lines of code, read the syntax carefully and decide if they are written correctly. Correct as many as you can.

a) concat [[1, 2, 3], [4, 5, 6]] -- yes
b) ++ [1, 2, 3] [4, 5, 6] -- no, need parens for prefix (++)
c) (++) "hello" " world" -- yes
d) ["hello" ++ " world] -- no, missing final quote.
e) 4 !! "hello" -- no, backwards
f) (!!) "hello" 4 -- yes
g) take "4 lovely" -- no, missing Int arg
h) take 3 "awesome" -- yes

2) Next we have two sets: the  rst set is lines of code and the other is a set of results. Read the code and  gure out which results came from which lines of code.

a) concat [[1 * 6], [2 * 6], [3 * 6]] -- [6, 12 18]
b) "rain" ++ drop 2 "elbow" -- rainbow
c) 10 * head [1, 2, 3] -- 10
d) (take 3 "Julie") ++ (tail "yes") -- Jules


Building Functions
==================

1 & 2) Given the list-manipulation functions mentioned in this chapter, write functions that take the following inputs and return the expected outputs.

a) "Curry is awesome" -> "Curry is awesome!"

> yell :: String -> String
> yell = (++ "!")

b) "Curry is awesome!" -> "y"

> drop4take1 :: String -> String
> drop4take1 = take 1 . drop 4

c) "Curry is awesome!" -> "awesome!"

> drop9 :: String -> String
> drop9 = drop 9

3) Write a function of type String -> Char which returns the third character in a String.

> thirdLetter :: String -> Char
> thirdLetter = (!! 2)

4) Now change that function so the string operated on is always the same and the variable represents the number of the letter you want to return.

> letterIndex :: Int -> Char
> letterIndex = ("Curry is awesome!" !!)

5) Using the `take` and `drop` functions… see if you can write a function called `rvrs`…. rvrs should take the string “Curry is awesome” and return the result “awesome is Curry.” This may not be the most lovely Haskell code you will ever write, but it is quite possible using only what we’ve learned so far. First write it as a single function in a source file. This doesn’t need to, and shouldn’t, work for reversing the words of any sentence. You’re expected only to slice and dice this particular string with `take` and `drop`.

> rvrs :: String -> String
> rvrs s = awesome ++ space ++ is ++ space ++ curry
>   where space = " "
>         curry = take 5 s
>         is = take 2 $ drop 6 s
>         awesome = drop 9 s

> rvrs2 = unwords . foldl (flip (:)) [] . words -- generalized

6. Let’s see if we can expand that function into a module.

(_See file_)
