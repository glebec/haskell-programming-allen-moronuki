# 16. Functor

A nice aligned snippet showing how functor `fmap` is elevated function application:

```hs
(<$>) :: Functor f => (a -> b) -> f a -> f b
($)   :: Functor f => (a -> b) ->   a ->   b
```

## 16.4 Exercises

1. kind of `a` in `a -> a` is `*`
2. kind of `b` and `T` in `a -> b a -> T (b a)` are both `* -> *`
3. kind of `c` in `c a b -> c b a` is `* -> * -> *`

