# 26. Monad Transformers

For some unknown monad `m`, a monad transformer named after a known monad `t` is a datatype `m t a` with functor / applicative / monad instances. Putting it another way, a monad transformer `KnownT` deals with `Known` embedded in an unknown monad.

- The "inner" monad is the known one, so we know how to "get at" the `a` (e.g. by casing on `Just` or `Nothing`).
- The "outer" monad is unknown, so all we can do is use generic functions like `bind` on it.
- The hard-coded transformer part is only needed for the `Monad` instance.

```hs
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

-- map the `a` "inside" the m-t stack
instance Functor m => Functor (MaybeT m) where
    fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

-- apply an `f` "inside" an m-t stack
instance Applicative m => Applicative (MaybeT m) where
    pure = MaybeT . pure . pure
    (<*>) (MaybeT mtf) (MaybeT mta) = MaybeT $ fmap (<*>) mtf <*> mta

-- the `m` and `t` don't change, but the `a` does
instance Monad m => Monad (MaybeT m) where
    (>>=) (MaybeT mta) f = MaybeT $ do
        ta <- mta
        case ta of -- the unique part of the transformer
            Nothing -> pure Nothing
            Just x -> runMaybeT (f x)
```

- we need a transformer for each known `t` type as there is no generic way to compose two monads.

## Recovering an ordinary type

`SomeMonadT IdentityMonad` is equivalent to `SomeMonad`. Could be useful in cases where you don't have direct access to the underlying monad.

## Examples

```hs
newtype IdentityT  m a = IdentityT { runIdentityT ::  m a }
newtype MaybeT     m a = MaybeT { runMaybeT ::        m (Maybe a) }
newtype ExceptT  e m a = ExceptT { runExceptT ::      m (Either e a) }
newtype ReaderT  r m a = ReaderT { runReaderT :: r -> m a }
newtype StateT   s m a = StateT { runStateT ::   s -> m (a, s) }
newtype WriterT  w m a = WriterT { runWriterT ::      m (a, w) }
newtype RWST r w s m a = RWST { runRWST ::  r -> s -> m (a, s, w) }
```

All of the function form versions (e.g. `r -> m a`) _could_ be expressed with the monad on the "outside" (e.g. `m (r -> a)`). However, in practice it is more convenient to bury the monad in the return type as much as possible, since it means you can supply "vanilla" arguments.

The monad `m` is buried as deeply as possible in the argument order so that `MonadTrans` instances can be defined, e.g. `instance MonadTrans (ExceptT e)`.

## `MonadTrans`

```hs
class MonadTrans t where
    -- lift from the argument monad to the constructed monad
    lift :: Monad m => m a -> t m a
```

> _"The critical thing to realize here is that lifting means you’re embedding an expression in a larger context by adding structure that doesn’t do anything."_

Note that a monad transformer "inserts" the named monad into the argument monad (e.g via `liftM` or `fmap`), but then "wraps" the result in the transformer instance (to give new `>>=` definitions etc.). For example:

```hs
instance MonadTrans MaybeT where
    lift = MaybeT . liftM Just
--         wrap   . inject named structure
```

## `MonadIO`

`liftIO` designed for lifting through an arbitrarily large stack of monad transformers whose outermost layer is `IO`.

```hs
-- Control.Monad.IO.Class
class (Monad m) => MonadIO m where
    liftIO :: IO a -> m a
```

Sample code snippets:

```hs
-- example instance
instance MonadIO m => MonadIO (EitherT e m) where
    liftIO = lift . liftIO

-- a program containing a reader for a state func yielding possible exceptions
liftIO :: IO a -> ExceptT e (StateT s (ReaderT r IO)) a
```

Laws

- `liftIO . return = return`
- `liftIO (m >>= f) = liftIO m >>= (liftIO . f)`

## Misc

Use the `transformers` library (including `ExceptT`)

### ReaderT

```hs
ask  :: Monad m =>             ReaderT r m r
asks :: Monad m => (r -> a) -> ReaderT r m a
```

### StateT

```hs
-- Fetch the current value of the state within the monad.
get :: Monad m => StateT s m s

-- modify f is an action that updates the state to the result of applying f to the current state.
modify :: Monad m => (s -> s) -> StateT s m ()

-- Evaluate a state computation with the given initial state and return the final value, discarding the final state.
evalStateT :: Monad m => StateT s m a -> s -> m a
```
