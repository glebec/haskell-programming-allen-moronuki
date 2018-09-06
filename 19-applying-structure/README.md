# 19. Applying Structure

A minimal `scotty` web server:

```hs
{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty

main :: IO ()
main = scotty 3000 $ do
    get "/" $
        html "try going to `/______` (fill in the blank with a verb!)"
    get "/:verb" $ do
        beam <- param "verb"
        html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
```
