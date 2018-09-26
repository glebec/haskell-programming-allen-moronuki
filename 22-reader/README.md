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

- the functor transforms the _result_ of a function
- the applicative uses a _partially-applied_ function (with `r`) on a _mapped_ `r`. Both `r` and `g r` become inputs to the binary func.
- the monad feeds the result of _mapping_ `r` into a function _with_ `r`. Both `r` and `g r` become inputs to the binary func, but in the other order.

In the latter two cases, a binary func and unary func are combined into a unary func waiting for `r`. The `r` is both used directly and also to make `a`.

```hs
ex :: (r -> a) >>=
 (a -> r -> b)

r -> a
      \__ b
      /
     r

ex :: (r -> b)
```

Successive binding of binary funcs results in distributing or sharing `r` among all the funcs in the chain (`r` is the same value everywhere here):

```hs
ex :: (r -> a) >>=
 (a -> r -> b) >>=
 (b -> r -> c) >>=
 (c -> r -> d)

r -> a
      \__ b
      /    \__ c
     r     /    \__ d
          r     /
               r

ex :: (r -> d)
```

### Scratch

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

## Reader

```hs
newtype Reader r a = Reader { runReader :: r -> a }
```

Basically just gives a name to the context `r ->` (where the result `a` can e.g. be mapped over, etc.).

```hs
(<$>) ::          (a -> b) -> Reader r a -> Reader r b
(<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
(=<<) :: (a -> Reader r b) -> Reader r a -> Reader r b
```
