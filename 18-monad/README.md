# 18. Monad

```hs
class Applicative m => Monad m where
    (>>=)  :: m a -> (a -> m b) -> m b -- minimal definition
    (>>)   :: m a ->       m b  -> m b
    return :: a -> m a -- same as `pure`
```

> "Given that `Monad` is stronger than `Applicative`, and `Applicative` is stronger than `Functor`, you can derive `Applicative` and `Functor` in terms of `Monad`"

```hs
f  <$> ma = return . f =<< ma
mf <*> ma = mf >>= (\f -> ma >>= (\a -> return $ f a))
```

Some type comparisons for fun:

```hs
( $ ) ::                    (a ->   b) ->   a ->   b
(<$>) :: Functor f     =>   (a ->   b) -> f a -> f b
(<*>) :: Applicative f => f (a ->   b) -> f a -> f b
(=<<) :: Monad f       =>   (a -> f b) -> f a -> f b
```

- functors: lift a vanilla function to working on structures
- applicatives: when you also have function(s) _in_ a structure
- monads: when you also have a function that _produces_ structure

Essence of monads is really in `join`:

```hs
join :: Monad m => m (m a) -> m a
join mma = mma >>= id
```

The monad lifts (`liftM`, `liftM2` etc.) are the same as the applicative lifts (`liftA`, `liftA2` etc.).

Kleisli composition (available in `Control.Monad`):

```hs
(>=>)    :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
flip (.) ::            (a ->   b) -> (b ->   c) -> a ->   c
```

Monad Laws (in terms of Kleisli composition):

```
pure >=> f          = f
         f >=> pure = f
f >=> (g >=> h) = (f >=> g) >=> h
```
