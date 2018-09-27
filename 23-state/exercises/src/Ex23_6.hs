{-# LANGUAGE InstanceSigs #-}

module Ex23_6 where

newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
    fmap f (Moi s2as) = Moi $ \s -> let (a, s') = s2as s in (f a, s')

instance Applicative (Moi s) where
    pure a = Moi $ \s -> (a, s)
    (<*>) (Moi s2fs) (Moi s2xs) = Moi $
        \s -> let (f, s1) = s2fs s
                  (x, s2) = s2xs s1
              in (f x, s2)

instance Monad (Moi s) where
    (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
    (>>=) (Moi s2as) a2Msb = Moi $
        \s -> let (a, s')  = s2as s
              in  runMoi (a2Msb a) s'
