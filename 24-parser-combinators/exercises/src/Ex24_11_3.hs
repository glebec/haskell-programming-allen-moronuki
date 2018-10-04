module Ex24_11_3 where

import Text.Trifecta
import Data.Maybe
import Ex24_11_2

-- 3. handle ±

base10Integer' :: Parser Integer
base10Integer' = do
    neg <- optional (char '-')
    int <- base10Integer
    pure $ if isJust neg
           then -int
           else int
