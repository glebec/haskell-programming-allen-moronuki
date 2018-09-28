# 24. Parser Combinators

## Misc

```hs
{-# LANGUAGE QuasiQuotes #-}

import Text.RawString.QQ

example :: String
example = [r|
123
abc
456
def
|]
```

```
:set -ddump-splices
:l code/example.hs
...splicing expression...
"\n123\nabc ...etc."
```
