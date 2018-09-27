# 23. State

```hs
newtype State s a = State { runState :: s -> (a, s) }
```
