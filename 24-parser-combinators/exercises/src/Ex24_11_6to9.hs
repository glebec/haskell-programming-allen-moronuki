module Ex24_11_6to9 where

import Control.Applicative
import Data.Word
import Data.Bits
import Data.Ix (range)
import Text.Trifecta

-- 6. IPv4

{-
https://tools.ietf.org/html/draft-main-ipaddr-text-rep-00

3.1 IPv4 Dotted Octet Format

A 32-bit IPv4 address is divided into four octets. Each octet is
represented numerically in decimal, using the minimum possible number
of digits (leading zeroes are not used, except in the case of 0
itself). The four encoded octets are given most-significant first,
separated by period characters.

ABNF: http://www.rfc-editor.org/rfc/rfc5234.txt

    IPv4address = d8 "." d8 "." d8 "." d8

    d8          = DIGIT               ; 0-9
                / %x31-39 DIGIT       ; 10-99
                / "1" 2DIGIT          ; 100-199
                / "2" %x30-34 DIGIT   ; 200-249
                / "25" %x30-35        ; 250-255
-}

newtype IPAddress = IPAddress Word32 deriving (Eq, Ord, Show)

str :: Parser Char -> Parser String
str = fmap (:[])

parseOctet :: (Read a, Num a) => Parser a
parseOctet = read <$>
    ( try (string "25" <> str (oneOf ['0'..'5']))
  <|> try (string "2" <> str (oneOf ['0'..'4']) <> str digit)
  <|> try (string "1" <> str digit <> str digit)
  <|> try (str (oneOf ['1'..'9']) <> str digit)
  <|> str digit)

ipv4 :: Parser IPAddress
ipv4 = do
    d1 <- parseOctet <* char '.'
    d2 <- parseOctet <* char '.'
    d3 <- parseOctet <* char '.'
    d4 <- parseOctet
    pure $ IPAddress $ (d1 `shiftL` 24) +
                       (d2 `shiftL` 16) +
                       (d3 `shiftL` 8)  +
                        d4

-- 7. IPv6

data IPAddress6 = IPAddress6 Word64 Word64 deriving (Eq, Ord, Show)

-- 8. Write Show instances which render the typical formats for IPs

octets :: Word32 -> [Word8]
octets w =
    [ fromIntegral (w `shiftR` 24)
    , fromIntegral (w `shiftR` 16)
    , fromIntegral (w `shiftR` 8)
    , fromIntegral w
    ]

formatIPv4 :: IPAddress -> String
formatIPv4 (IPAddress n) = let os = octets n in
                           show (head os) ++ "." ++
                           show (os !! 1) ++ "." ++
                           show (os !! 2) ++ "." ++
                           show (os !! 3)

-- 9. Write `ip4to6`
