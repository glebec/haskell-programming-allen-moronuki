{-# OPTIONS_GHC -Wall #-}

module ScratchCh6 where

-- Example trivialities

data Trivial = Trivial -- type on left, data constructor(s) on right

instance Eq Trivial where
    Trivial == Trivial = True

instance Show Trivial where
    show Trivial = "Trivial"


-- A sum type

data DayOfWeek =
    Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Ord, Show) -- deriving Eq

instance Eq DayOfWeek where
    (==) Mon Mon = True
    (==) Tue Tue = True
    (==) Wed Wed = True
    (==) Thu Thu = True
    (==) Fri Fri = True
    (==) Sat Sat = True
    (==) Sun Sun = True
    (==) _   _   = False

-- A product type

data Date =
    Date DayOfWeek Int deriving Show -- deriving Eq -- day date

instance Eq Date where
    (==) (Date day date) (Date day' date') = day == day' && date == date'

-- A type with polymorphic parameters

data Identity a = Identity a -- HLint suggests using newtype.

--      `Eq a =>` added to specify that `a` must be of type `Eq`
instance Eq a => Eq (Identity a) where
    (==) (Identity v) (Identity v') = v == v'

-- Can we define cyclic Ord instances?

data Player = Rock | Paper | Scissors deriving (Eq, Enum, Show)

instance Ord Player where
    compare Paper Rock = GT
    compare Rock Scissors = GT
    compare Scissors Paper = GT
    compare Rock Paper = LT
    compare Scissors Rock = LT
    compare Paper Scissors = LT
    compare _ _ = EQ
