{-# OPTIONS_GHC -W #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ScratchCh11 where

class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 42

newtype Goats = Goats Int deriving (Eq, Show, TooMany) -- added TooMany

-- instance TooMany Goats where
--     tooMany (Goats n) = n > 42

data Fiction    = MkFiction    deriving Show
data Nonfiction = MkNonfiction deriving Show

data BookType = FictionBook    Fiction
              | NonfictionBook Nonfiction
              deriving Show

type AuthorName = String
-- a product over a sum can be re-written as a sum of products
-- data Author = Author (AuthorName, BookType) deriving Show
data Author = Fiction    AuthorName
            | Nonfiction AuthorName deriving (Eq, Show)

data Expr =
    Number Int
  | Add Expr Expr
  | Minus Expr
  | Mult Expr Expr
  | Divide Expr Expr

-- some experimenting…
data Weave = Circle | Round | Thrice deriving (Eq, Show) -- sum type size 3
data Eyes = Holy | Dread deriving (Eq, Show) -- sum type size 2
data HoneyDew = Paradise (Weave -> Eyes) -- exponent type size 2^3 = 8
drink (Paradise f) = f -- function extractor / applicator
milk = Paradise (const Holy) -- example HoneyDew
result = drink milk Thrice -- Holy

-- _ -> Holy
-- _ -> Dread
-- (Circle/Round -> Holy, Thrice -> Dread)
-- (Circle/Thrice -> Holy, Round -> Dread)
-- (Round/Thrice -> Holy, Circle -> Dread)
-- (Circle/Round -> Dread, Thrice -> Holy)
-- (Circle/Thrice -> Dread, Round -> Holy)
-- (Round/Thrice -> Dread, Circle -> Holy)

-- Intuition: group 1
-- f1 Circle = Holy (1 of 2)
-- f1 Round = Holy (1 of 2)
-- f1 Thrice = Holy (1 of 2)
-- group 2
-- f1 Circle = Holy (1 of 2)
-- f1 Round = Holy (1 of 2)
-- f1 Thrice = Dread (2 of 2)
-- ...for each input (3 cases) there are two choices, = 2 * 2 * 2 possibilities
