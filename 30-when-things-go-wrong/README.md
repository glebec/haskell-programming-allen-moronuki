# 30. When Things Go Wrong

A polymorphic and extensible exception type.

```hs
class (Typeable e, Show e) => Exception e where
    toException      :: e -> SomeException
    fromException    :: SomeException -> Maybe e
    displayException :: e -> String
    -- ...
```

To create `SomeException`, you need a value that fulfills the `Exception` constraint.

```hs
-- via existential quantification
data someException =
    forall e . Exception e => SomeException e

-- via GADT syntax
data SomeException where
    SomeException :: Exception e => e -> SomeException
```

Above lets us extend the error type, rather than needing to write a single sum type of all possible errors.


## Catching

Attach an error handler to an `IO` action in order to get back on track.

```hs
catch :: Exception e
      => IO a
      -> (e -> IO a)
      -> IO a
```

```hs
cast :: (Typeable a, Typeable b) => a -> Maybe b
```

> _At runtime, when an exception is thrown, it starts rolling back through the stack, looking for a `catch`. When it finds a `catch`, it checks to see what type of exception this `catch` catches. It calls `fromException` and `cast` to check if the type of the exception that got thrown matches the type of an exception we’re handling with the `catch`._

- Catching can only occur in `IO`.
- Will only catch the hierarchy of exceptions that `e` includes; see below for more detailed explanation.


## Try

Convert an `IO` action into one which results in an explicit `Either`.

```hs
-- Control.Exception
try :: Exception e
    => IO a
    -> IO (Either e a)
```

Will catch exceptions of type `e` – if `e` is a `SomeException`, that is everything, but if not, it will only catch the subset of errors your type `e` represents. See [`Example_30_05`](Example_30_05.hs).


## Throw

Deliberately create an `IO` action with an exception.

```hs
throwIO :: Exception e
        => e
        -> IO a
```

Generally prefer `throwIO` which is explicit and typesafe, rather than `throw`, which is not.

> _Handling exceptions must be done in `IO` even if they were thrown without an `IO` type. You almost never want `throw` as it throws exceptions without any warning in the type, even `IO`._


## Catching Multiple Disparate Exception Types

Can use `catches` with a list of handlers to convert an `IO` action into one that handles multiple specific exception types.

```hs
-- Control.Exception
data Handler a = forall e. Exception e => Handler (e -> IO a)

catches :: IO a -> [Handler a] -> IO a
```

Or we can make a single sum type and make it an instance of `Exception`.

## Misc

This machinery is not for catching bottom (I imagine it would solve the halting problem if it always could!).

## Async Exceptions

Can `throwTo` to a specific thread by ID from the parent – cool.
