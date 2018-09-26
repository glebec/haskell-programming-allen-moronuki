{-# LANGUAGE InstanceSigs #-}

module Ex22_07 where

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
    fmap f (Reader ra) = Reader $ f . ra

instance Applicative (Reader r) where
    pure :: a -> Reader r a
    pure a = Reader $ const a
    (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
    (<*>) (Reader rab) (Reader ra) = Reader (\r -> rab r (ra r))

-- 1.

instance Monad (Reader r) where
    (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
    (>>=) (Reader ra) a2rb = Reader $ \r -> runReader (a2rb (ra r)) r

-- 2.

newtype HumanName = HumanName String deriving (Eq, Show)
newtype DogName   = DogName   String deriving (Eq, Show)
newtype Address   = Address   String deriving (Eq, Show)

data Person =
    Person {
      humanName :: HumanName
    , dogName :: DogName
    , address :: Address
    } deriving (Eq, Show)

data Dog =
    Dog {
      dogsName :: DogName
    , dogsAddress :: Address
    } deriving (Eq, Show)

getDogRM :: Reader Person Dog
getDogRM = do
    n <- Reader dogName
    a <- Reader address
    return $ Dog n a
