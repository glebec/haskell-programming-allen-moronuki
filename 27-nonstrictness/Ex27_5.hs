module Ex27_5 where

p1 = const 1 undefined
-- (\a b -> a) 1 undefined
-- 1

p2 = const undefined 1
-- (\a b -> a) undefined 1
-- undefined

p3 = flip const undefined 1
-- (\f a b -> f b a) const undefined 1
-- const 1 undefined
-- 1

p4 = flip const 1 undefined
-- undefined

p5 = const undefined undefined
-- (\a b -> a) undefined undefined
-- undefined

p6 = foldr const 'z' ['a'..'e']
-- foldr const 'z' (enumFromTo 'a' 'e')
-- case (enumFromTo 'a' 'e') of [] -> z; (x:xs) -> const 'a' (foldr const 'z' xs)
-- const 'a' ...
-- 'a'

p7 = foldr (flip const) 'z' ['a'..'e']
-- foldr (flip const) 'z' (enumFromTo 'a' 'e')
-- case (enumFromTo 'a' 'e') of [] -> z; (x:xs) -> (flip const) 'a' (foldr const 'z' xs)
-- flip const 'a' (foldr (flip const) 'z' ['b'..'e'])
-- foldr (flip const) 'z' ['b'..'e']
-- ...
-- 'z'
