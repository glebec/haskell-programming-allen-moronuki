module Ex24_11_2 where

import Text.Trifecta

-- 2. Write a parser for pos ints.

parseDigit :: Parser Char
parseDigit = oneOf ['0'..'9']

base10Integer :: Parser Integer
base10Integer = read <$> some parseDigit
