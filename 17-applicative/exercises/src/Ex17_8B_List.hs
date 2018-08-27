module Ex17_8B_List where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- List Applicative

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Semigroup (List a) where
    Nil <> ys = ys
    Cons x xs <> ys = Cons x $ xs <> ys

instance Applicative List where
    pure x = Cons x Nil
    (<*>) fs xs = flatMap (<$> xs) fs
    -- (<*>) (Cons f fs) xs = (f <$> xs) <> (fs <*> xs)
    -- (<*>) _ _ = Nil

instance Eq a => EqProp (List a) where
    (=-=) = eq

genList :: Arbitrary a => Gen (List a)
genList = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return Nil), (3, return $ Cons a b)]

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = genList

checkList = quickBatch $ applicative (undefined :: List (String, String, Int))

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold (<>) Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap _ Nil = Nil
flatMap f xs = (concat' . fmap f) xs

take' :: Int -> List a -> List a
take' _ Nil = Nil
take' 0 _   = Nil
take' n (Cons x xs) = Cons x $ take' (n - 1) xs

-- repeat' :: a -> List a
-- repeat' x = let xs = Cons x xs in xs

zipWith' :: (a -> b -> c) -> List a -> List b -> List c
zipWith' _ Nil _ = Nil
zipWith' _ _ Nil = Nil
zipWith' f (Cons a as) (Cons b bs) = Cons (f a b) (zipWith' f as bs)

fromBaseList :: [a] -> List a
fromBaseList = foldr Cons Nil
