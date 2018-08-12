{-# LANGUAGE FlexibleInstances #-}

module ChapterExercises where -- 16.17

import GHC.Arr

-- Determine if a valid Functor can be written for the datatype provided.

-- 1.

data Bool' = False' | True'

-- cannot make a functor, the kind of Bool is TYPE

-- 2.

data BoolAndSomethingElse a = False'' a | True'' a

instance Functor BoolAndSomethingElse where
    fmap f (False'' a) = False'' $ f a
    fmap f (True'' a)  = True'' $ f a

-- 3.

data BoolAndMaybeSomethingElse a = Falsish | Truish a

instance Functor BoolAndMaybeSomethingElse where
    fmap _ Falsish = Falsish
    fmap f (Truish a) = Truish $ f a

-- 4.

newtype Mu f = InF { outF :: f (Mu f) }

-- cannot define a Functor instance, Mu has kind (TYPE -> TYPE) -> TYPE

-- 5.

data D = D (Array Word Word) Int Int

-- cannot define a Functor instance, D has kind TYPE

-- Rearrange the arguments to the type constructor of the datatype so the
-- Functor instance works.

-- 1.

data Sum' b a = First' a | Second' b

instance Functor (Sum' e) where
    fmap f (First' a)  = First' (f a)
    fmap f (Second' b) = Second' b

-- 2.

data Company a c b = DeepBlue a c | Something b

instance Functor (Company e e') where
    fmap f (Something b) = Something (f b)
    fmap _ (DeepBlue a c) = DeepBlue a c

-- 3.

data More b a = L a b a
              | R b a b
              deriving (Eq, Show)

instance Functor (More x) where
    fmap f (L a b a') = L (f a) b (f a')
    fmap f (R b a b') = R b (f a) b'

-- Write Functor instances for the following datatypes.

-- 1.

data Quant a b = Finance | Desk a | Bloor b

instance Functor (Quant a) where
    fmap _ Finance   = Finance
    fmap _ (Desk a)  = Desk a
    fmap f (Bloor b) = Bloor $ f b

-- 2.

data K a b = K a

instance Functor (K a) where
    fmap _ (K a) = K a

-- 3.

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

instance Functor (Flip K a) where
    fmap f (Flip (K b)) = Flip (K $ f b)

-- 4.

data EvilGoateeConst a b = GoatyConst b

instance Functor (EvilGoateeConst a) where
    fmap f (GoatyConst b) = GoatyConst $ f b

-- 5. Do you need something extra to make the instance work?

data LiftItOut f a = LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
    fmap f (LiftItOut f') = LiftItOut $ fmap f f'

-- 6.

data Parappa f g a = DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap f (DaWrappa f' g') = DaWrappa (f <$> f') (f <$> g')

-- 7.

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

instance Functor g => Functor (IgnoreOne f g a) where
    fmap f (IgnoringSomething f' g') = IgnoringSomething f' (f <$> g')

-- 8.

data Notorious g o a t = Notorious (g o) (g a) (g t)

instance Functor g => Functor (Notorious g o a) where
    fmap f (Notorious go ga gt) = Notorious go ga (f <$> gt)

-- 9.

data List' a = Nil' | Cons' a (List' a)

instance Functor List' where
    fmap _ Nil' = Nil'
    fmap f (Cons' x xs) = Cons' (f x) (f <$> xs)

-- 10.

data GoatLord a = NoGoat
                | OneGoat a
                | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor GoatLord where
    fmap _ NoGoat = NoGoat
    fmap f (OneGoat a) = OneGoat $ f a
    fmap f (MoreGoats g g' g'') = MoreGoats (f <$> g) (f <$> g') (f <$> g'')

-- 11.

data TalkToMe a = Halt
                | Print String a
                | Read (String -> a)

instance Functor TalkToMe where
    fmap _ Halt = Halt
    fmap f (Print s a) = Print s (f a)
    fmap f (Read sa) = Read $ f . sa
