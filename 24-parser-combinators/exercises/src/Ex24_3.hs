module Ex24_3 where

import Text.Trifecta
import Text.Parser.Combinators

stop :: Parser a
stop = unexpected "stop"

-- 1.

oneEof :: Parser Char
oneEof = char '1' >>= \c -> eof >> return c

oneTwoEof :: Parser Char
oneTwoEof = char '1' >> char '2' >>= \c -> eof >> return c

-- 2.

oneTwoThree :: Parser String
oneTwoThree = string "123"

oneTwoThreeStop :: Parser String
oneTwoThreeStop = string "123" >> stop

-- 3.

myString :: String -> Parser String
myString "" = mempty
myString s@(c:cs) = char c >> myString cs >> return s

-- Tests

testParseOn :: Show a => Parser a -> String -> IO ()
testParseOn p s =
    print $ parseString p mempty s

pNL :: String -> IO ()
pNL s = putStrLn $ '\n' : s

demo24_3 = do
    pNL "oneEof \"1\":"
    testParseOn oneEof "1"
    pNL "oneEof \"12\":"
    testParseOn oneEof "12"
    pNL "oneTwoEof \"12\":"
    testParseOn oneTwoEof "12"
    pNL "oneTwoEof \"123\":"
    testParseOn oneTwoEof "123"
    pNL "oneTwoThree \"12\":"
    testParseOn oneTwoThree "12"
    pNL "oneTwoThree \"12345\":"
    testParseOn oneTwoThree "12345"
    pNL "oneTwoThreeStop \"12345\":"
    testParseOn oneTwoThreeStop "12345"
    pNL "myString \"\" on \"\":"
    testParseOn (myString "") ""
    pNL "myString \"hi\" on \"\":"
    testParseOn (myString "hi") ""
    pNL "myString \"hi\" on \"hiya\":"
    testParseOn (myString "hi") "hiya"
