module Ex23_8 where

import Ex23_6

-- 1.

get' :: Moi s s
get' = Moi $ \s -> (s, s)

-- 2.

put' :: s -> Moi s ()
put' s = Moi $ const ((), s)

-- 3.

exec' :: Moi s a -> s -> s
exec' (Moi s2as) s = snd $ s2as s

-- 4.

eval' :: Moi s a -> s -> a
eval' (Moi s2as) s = fst $ s2as s

-- 5.

myModify :: (s -> s) -> Moi s ()
myModify f = Moi $ \s -> ((), f s)
