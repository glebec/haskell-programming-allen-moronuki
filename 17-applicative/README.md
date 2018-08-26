# 17. Applicative

```hs
class Functor f => Applicative f where
    pure  :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b -- aka apply / ap
```

```hs
(<$>) ::     Functor f =>   (a -> b) -> f a -> f b
(<*>) :: Applicative f => f (a -> b) -> f a -> f b
```

```hs
import Control.Applicative

liftA  :: Applicative f => (a -> b)           -> f a -> f b -- basically fmap
liftA2 :: Applicative f => (a -> b -> c)      -> f a -> f b -> f c
liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
```

---

Applicative functors are monoidal functors.

```hs
(<>)  :: Monoid a      => a          -> a   -> a
(<*>) :: Applicative f => f (a -> b) -> f a -> f b
```

---

Some applicative instances.

```hs
("Woo", (+1)) <*> (" hoo", 4) -- ("Woo hoo", 5) -- apply second

Just (+1) <*> Nothing -- Nothing -- apply if exists

[(+2), (*3)] <*> [3, 4] -- [5, 6, 9, 12] -- cartesian product

(++) <$> getLine <*> getLine -- line1 ++ line2 –– apply to program outputs

Right (+1) <*> Right 5 -- Right 6, apply if exists

Left ":(" <*> Left "X|" -- Left ":(", monoid uses first Left value

Failure e1 <*> Failure e2 -- Failure $ e1 <> e2, Validation data type
```

---

Two ways to "start" an applicative:

- Mapping a function onto arguments in an applicative context, in order to apply it to more arguments
- Lifting the function into the applicative with a utility function (which just does the above)

```hs
(,) <$> [1, 2] <*> [3, 4] -- [(1, 3), (1, 4), (2, 3), (2, 4)]
liftA2 (,) [1, 2] [3, 4] -- ditto
```

---

A common reason one might end up wanting an applicative: multiple arguments in a context. If you have a `Just x` and a `Just y` and want a `Just $ x * y`, it's relatively easy to `liftA2 (*)` the two Justs.

## Laws

- identity: `pure id <*> v == v`
- composition: `pure (.) <*> u <*> v <*> w == u <*> (v <*> w)`
    - note this implies `u` and `v` are applicatives of composable functions
- homomorphism: `pure f <*> pure x == pure (f x)`
- interchange: `u <*> pure y == pure ($ y) <*> u`
