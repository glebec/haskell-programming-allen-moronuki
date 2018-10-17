# 25. Composing Types

```hs
newtype Compose f g a =
    Compose { getCompose :: f (g a) }
    deriving (Eq, Show)
```

```
> :k Compose
Compose :: (* -> *) -> (* -> *) -> * -> *

> :t Compose [Just 'x']
Compose [Just 'x'] :: Compose [] Maybe Char
```

```hs
instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f (Compose fg) = Compose $ (fmap . fmap) f fg
```
