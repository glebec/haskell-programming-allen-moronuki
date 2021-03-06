module Ex24_11_6to9 where

import Control.Applicative
import Data.Word
import Data.Bits
import Data.Char (digitToInt, intToDigit)
import Data.Foldable (foldl', foldl1, toList)
import Data.List (unfoldr, intercalate)
import Data.Maybe (catMaybes)
import Text.Trifecta
import Test.Hspec

-- 6. IPv4

{-
https://tools.ietf.org/html/draft-main-ipaddr-text-rep-00#section-3.1

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

newtype IPAddress = IPAddress { getWord :: Word32 } deriving (Eq, Ord, Show)

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

{-
https://tools.ietf.org/html/draft-main-ipaddr-text-rep-00#section-3.2

3.2 IPv6 Presentation Format

A 128-bit IPv6 address is divided into eight 16-bit pieces. Each
piece is represented numerically in case-insensitive hexadecimal,
using one to four hexadecimal digits (leading zeroes are permitted).
The eight encoded pieces are given most-significant first, separated
by colon characters. Optionally, the least-significant two pieces
may instead be represented in IPv4 address textual format (the
<IPv4address> production given above). Optionally, once in the
address, a sequence of one or more consecutive zero-valued 16-bit
pieces may be elided, omitting all their digits and leaving exactly
two consecutive colons in their place to mark the elision.

ABNF: http://www.rfc-editor.org/rfc/rfc5234.txt

IPv6address =                           6(h16 ":") ls32
            /                      "::" 5(h16 ":") ls32
            / [ 0*0(h16 ":") h16 ] "::" 4(h16 ":") ls32
            / [ 0*1(h16 ":") h16 ] "::" 3(h16 ":") ls32
            / [ 0*2(h16 ":") h16 ] "::" 2(h16 ":") ls32
            / [ 0*3(h16 ":") h16 ] "::" 1(h16 ":") ls32
            / [ 0*4(h16 ":") h16 ] "::" 0(h16 ":") ls32
            / [ 0*5(h16 ":") h16 ] "::"             h16
            / [ 0*6(h16 ":") h16 ] "::"

ls32        = h16 ":" h16 / IPv4address

h16         = 1*4HEXDIG
-}

data IPAddress6 = IPAddress6 Word64 Word64 deriving (Eq, Ord, Show)

hex :: Num a => Parser a
hex = fromIntegral . digitToInt <$>
      (oneOf (['A'..'F'] ++ ['a'..'f']) <|> digit)

accumHex :: Num a => [a] -> a
accumHex = foldl' (\t d -> t * 16 + d) 0

h16 :: Num a => Parser a
h16 = do
    h <- hex
    hs <- count 3 (optional hex)
    pure $ accumHex (h : catMaybes hs)

ls32 :: Num a => Parser a
ls32 =
    try (do
        hh <- h16
        colon
        hl <- h16
        pure $ hh * 16^4 + hl
    ) <|> (fromIntegral . getWord <$> ipv4)

upTo :: Int -> Parser a -> Parser [a]
upTo 0 _ = pure []
upTo n p = (:) <$> try p <*> upTo (n - 1) p <|> pure []

sepByUpTo :: Parser a -> Int -> Parser b -> Parser [a]
sepByUpTo p n sep
    | n == 0 = pure []
    | n == 1 = toList <$> optional p
    | otherwise = do
        x <- optional p
        case x of
            Nothing -> pure []
            (Just a) -> do
                as <- upTo (n - 1) (sep >> p)
                pure $ a:as

h16part :: Num a => Parser a
h16part = h16 <* colon

accumW16 :: Num a => [a] -> a
accumW16 = foldl' (\t n -> t * 2^16 + n) 0

ipv6simple :: Num a => Parser a
ipv6simple = do
    h16s <- count 6 h16part
    w32 <- ls32
    pure $ accumW16 h16s * 2^32 + w32

ipv6preElision :: Num a => Int -> Parser a
ipv6preElision n = do
    h16s <- (h16 `sepByUpTo` n) colon <* string "::"
    pure $ accumW16 h16s * 2^(128 - 16 * length h16s)

ipv6elide32 :: Num a => Int -> Parser a
ipv6elide32 n = do
    high <- ipv6preElision (5 - n)
    l16s <- count n h16part
    w32 <- ls32
    pure $ high + (accumW16 l16s * 2^32) + w32

ipv6elide16 :: Num a => Parser a
ipv6elide16 = liftA2 (+) (ipv6preElision 5) h16

ipv6 :: Parser IPAddress6
ipv6 = intToIP6 <$>
    ( try (ipv6simple :: Parser Integer) <|>
      foldl1 (<|>) (try . ipv6elide32 <$> [5,4..0]) <|>
      try ipv6elide16 <|>
      ipv6preElision 6 )

-- 8. Write Show instances which render the typical formats for IPs

-- ed.: I prefer to keep Show instances as valid Haskell, not pretty-printing

getBits :: (Num a, FiniteBits a) => Int -> Int -> a -> a
getBits s n b
    | s < 0 || len <= s || len <= end = 0
    | otherwise = go 0 0
    where end = s + n
          len = finiteBitSize b
          go i x = if s + i == end
                   then x
                   else go (i + 1) (x + if b `testBit` (s + i) then 2^i else 0)

chunks :: (Integral a, FiniteBits a, Num a) => Int -> a -> [a]
chunks s w = unfoldr cons (finiteBitSize w) where
    cons 0 = Nothing
    cons n = Just (getBits (n - s) s w, n - s)

formatIPv4 :: IPAddress -> String
formatIPv4 (IPAddress n) = let os = chunks 8 n in
    show (head os) ++ "." ++
    show (os !! 1) ++ "." ++
    show (os !! 2) ++ "." ++
    show (os !! 3)

w64toString :: Word64 -> String
w64toString = intercalate ":" .
              group4 .
              fmap (intToDigit . fromIntegral) .
              chunks 4

group4 :: [a] -> [[a]]
group4 [] = []
group4 xs = take 4 xs : group4 (drop 4 xs)

formatIPv6 :: IPAddress6 -> String
formatIPv6 (IPAddress6 h l) = w64toString h ++ ":" ++ w64toString l

-- 9. Write `ip4to6`

ip4to6 :: IPAddress -> IPAddress6
ip4to6 (IPAddress w32) = IPAddress6 0 (fromIntegral w32)

ip4to6s :: String -> Maybe String
ip4to6s ip4 = case parseString ipv4 mempty ip4 of
    Success a -> Just $ show (ip4to6 a)
    _         -> Nothing

-- tests

runIP6 :: String -> Result IPAddress6
runIP6 = parseString ipv6 mempty

intToIP6 :: Integral a => a -> IPAddress6
intToIP6 n = IPAddress6 (fromIntegral $ n `div` 2^64) (fromIntegral n)

yields :: Eq a => a -> Result a -> Bool
yields s (Success s') = s == s'
yields _ _ = False

specParseYields :: String -> IPAddress6 -> SpecWith ()
specParseYields s ip =
    it ("parses " ++ s ++ " as " ++ show ip) $
        runIP6 s `shouldSatisfy` yields ip

checkIPeq :: Integral a => String -> a -> SpecWith ()
checkIPeq s n = specParseYields s (intToIP6 n)

checkIP :: IO ()
checkIP = hspec $
    describe "ipv6" $ do
        checkIPeq "0:0:0:0:0:ffff:ac10:fe01" 281473568538113
        checkIPeq "FE80:0000:0000:0000:0202:B3FF:FE1E:8329"
                  338288524927261089654163772891438416681
        checkIPeq "FE80::0202:B3FF:FE1E:8329"
                  338288524927261089654163772891438416681
        checkIPeq "2001:DB8:0000:0000:8:800:200C:417A"
                  42540766411282592856906245548098208122
        checkIPeq "2001:DB8::8:800:200C:417A"
                  42540766411282592856906245548098208122
        checkIPeq "::0.0.0.1" 1
        checkIPeq "1::0.0.0.1" 5192296858534827628530496329220097
        checkIPeq "1:0::0.0.0.1" 5192296858534827628530496329220097
        checkIPeq "1:0:0::0.0.0.1" 5192296858534827628530496329220097
        checkIPeq "1:0:0:0::0.0.0.1" 5192296858534827628530496329220097
        checkIPeq "1:0:0:0:0::0.0.0.1" 5192296858534827628530496329220097
        checkIPeq "1:0:0:0:0:0::" 5192296858534827628530496329220096
