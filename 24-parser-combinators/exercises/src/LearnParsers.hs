module LearnParsers where

import Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

one :: Parser Char
one = char '1'

one' :: Parser Char
one' = one >> stop

oneTwo :: Parser Char
oneTwo = char '1' >> char '2'

oneTwo' :: Parser Char
oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p =
    print $ parseString p mempty "123"

testParseOn :: Parser Char -> String -> IO ()
testParseOn p s =
    print $ parseString p mempty s

pNL :: String -> IO ()
pNL s = putStrLn $ '\n' : s

demoParsers = do
    pNL "stop:"
    testParse stop
    pNL "one:"
    testParse one
    pNL "one':"
    testParse one'
    pNL "oneTwo:"
    testParse oneTwo
    pNL "oneTwo':"
    testParse oneTwo'
