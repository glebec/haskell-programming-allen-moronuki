{-# LANGUAGE Strict #-}

module StrictList where

data List a = Nil | Cons a (List a) deriving Show

take' :: (Ord a, Num a) => a -> List x -> List x
take' n _ | n <= 0  = Nil
take' _ Nil         = Nil
take' n (Cons x xs) = Cons x (take' (n - 1) xs)

map' :: (a -> b) -> List a -> List b
map' _ Nil = Nil
map' f (Cons x xs) = Cons (f x) (map' f xs)

repeat' :: a -> List a
repeat' x = xs where xs = Cons x xs

main :: IO ()
main = do
    print $ take' 10 $ map' (+1) (Cons 5 Nil) -- terminates
    -- print $ take' 10 $ map' (+1) (repeat' 1) -- nonterminating
