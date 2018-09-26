# 22. Reader

## Function Review

The type `(->) r` (easier-read, though not valid syntax, as `r ->`) is monadic.

```hs
(<$>) :: Functor f     =>      (a ->      b) -> f     a  -> f     b
(<$>)                          (a ->      b) -> (r -> a) -> (r -> b) -- (.)
(<*>) :: Applicative f => f    (a ->      b) -> f     a  -> f     b
(<*>)                     (r -> a ->      b) -> (r -> a) -> (r -> b)
(=<<) :: Monad f       =>      (a -> f    b) -> f     a  -> f     b
(=<<)                          (a -> r -> b) -> (r -> a) -> (r -> b)
```

## Examples

```hs
dbl = (*2) :: Num a => a -> a
inc = (+1) :: Num a => a -> a

dThenI = i . d
dThenI = fmap i d

fmap (+) dbl :: Num a => a -> (a -> a) -- (+) :: a -> (a -> a)
-- result of `dbl` becomes first arg to `+`
-- arguments are (value to double, val to add)

((+) <$> (*2) <*> (+10)) :: Num b => b -> b
-- add result of *2 to result of +10
```
