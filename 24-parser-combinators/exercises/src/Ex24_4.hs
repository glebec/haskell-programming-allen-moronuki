{-# LANGUAGE OverloadedStrings #-}

module Ex24_4 where

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta

badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
    numerator <- decimal
    char '/'
    denominator <- decimal
    case denominator of
        0 -> fail "Denominator cannot be zero"
        _ -> return (numerator % denominator)

demoFraction :: IO ()
demoFraction = do
    let parseFraction' = parseString parseFraction mempty
    print $ parseFraction' shouldWork
    print $ parseFraction' shouldAlsoWork
    print $ parseFraction' badFraction
    print $ parseFraction' alsoBad

parseIntEof :: Parser Integer
parseIntEof = const <$> integer <*> eof
-- parseIntEof = integer >>= \i -> eof >> return i

demoIntEof :: IO ()
demoIntEof = print $ parseString parseIntEof mempty "123"
