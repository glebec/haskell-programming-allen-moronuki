# 31. Final Project

Wow, we made it.

## Finger Daemon

Server-side `fingerd` process for `finger` client program. Communication via TCP.

## Misc

```hs
{-# LANGUAGE RecordWildCards #-}

module RWCDemo where

data Blah = Blah { myThing :: Int }

wew Blah{..} = print myThing -- no need to destructure or apply accessor
```
