module Ex24_6 where

import Control.Applicative -- Alternative
import Data.Ratio ((%))
import Text.Trifecta
import Ex24_4 (parseFraction)

twoStringsToDouble :: String -> String -> Double
twoStringsToDouble a b =
    let intPart  = read a :: (Read a, Num a) => a
        scale    = 10 ^ fromIntegral (length b)
        fracPart = read b :: (Read a, Num a) => a
    in  intPart + fracPart / scale

parseDecimal :: Parser Double
parseDecimal = do
    intDigits <- some digit
    char '.'
    frcDigits <- some digit
    return $ twoStringsToDouble intDigits frcDigits

parseRational :: Parser (Either Double Rational)
parseRational = (Left <$> try parseDecimal) <|> (Right <$> try parseFraction)

parseNos :: Parser (Either Integer String)
parseNos = (Left <$> integer) <|> (Right <$> some letter)

demo24_6 :: IO ()
demo24_6 = do
    print $ parseString parseRational mempty "1/13"
    print $ parseString parseRational mempty "1.13"
