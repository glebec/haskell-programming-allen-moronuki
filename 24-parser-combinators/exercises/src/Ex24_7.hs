{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Ex24_7 where

-- INI format parsing

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Char (isAlpha)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Test.Hspec
import Text.RawString.QQ
import Text.Trifecta

-- headers

headerEx :: ByteString
headerEx = "[blah]"

newtype Header = Header String deriving (Eq, Ord, Show)

parseBracketPair :: Parser a -> Parser a
parseBracketPair p = char '[' *> p <* char ']'

parseHeader :: Parser Header
parseHeader = parseBracketPair (Header <$> some letter)

-- assignments

assignmentEx :: ByteString
assignmentEx = "woot=1"

type Name = String
type Value = String
type Assignments = Map Name Value

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

parseAssignment :: Parser (Name, Value)
parseAssignment = do
    name <- some letter
    char '='
    val <- some (noneOf "\n")
    skipEOL
    return (name, val)

-- comments

commentEx :: ByteString
commentEx = "; Last modified 1 April 2001 by Jane Doe"

commentEx' :: ByteString
commentEx' = "; blah\n; woot\n \n; hah"

skipComments :: Parser ()
skipComments = skipMany $ do
    char ';' <|> char '#'
    skipMany (noneOf "\n")
    skipEOL

-- sections

sectionEx :: ByteString
sectionEx = "; ignore me\n[states]\nChris=Texas"

sectionEx' :: ByteString
sectionEx' = [r|
; ignore me
[states]
Chris=Texas
|]

sectionEx'' :: ByteString
sectionEx'' = [r|
; comment
[section]
host=wikipedia.org
alias=claw

[whatisit]
red=intoothandclaw
|]

data Section = Section Header Assignments deriving (Eq, Show)

newtype Config = Config (Map Header Assignments) deriving (Eq, Show)

skipWhitespace :: Parser ()
skipWhitespace = skipMany (char ' ' <|> char '\n')

parseSection :: Parser Section
parseSection = do
    skipWhitespace
    skipComments
    h <- parseHeader
    skipEOL
    assignments <- some parseAssignment
    return $ Section h (M.fromList assignments)

-- post processing

rollup :: Section -> Map Header Assignments -> Map Header Assignments
rollup (Section h a) = M.insert h a

parseIni :: Parser Config
parseIni = do
    sections <- some parseSection
    let mapOfSections = foldr rollup M.empty sections
    return $ Config mapOfSections

-- tests

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

testParseIni :: IO ()
testParseIni = hspec $ do
    describe "Assignment Parsing" $
        it "can parse a simple assignment" $ do
            let m = parseByteString parseAssignment mempty assignmentEx
                r' = maybeSuccess m
            print m
            r' `shouldBe` Just ("woot", "1")
    describe "Header Parsing" $
        it "can parse a simple header" $ do
            let m = parseByteString parseHeader mempty headerEx
                r' = maybeSuccess m
            print m
            r' `shouldBe` Just (Header "blah")
    describe "Comment parsing" $
        it "Skips comment before header" $ do
            let p = skipComments >> parseHeader
                i = "; woot\n[blah]"
                m = parseByteString p mempty i
                r' = maybeSuccess m
            print m
            r' `shouldBe` Just (Header "blah")

    describe "Section parsing" $
        it "can parse a simple section" $ do
            let m = parseByteString parseSection mempty sectionEx
                r' = maybeSuccess m
                states = M.fromList [("Chris", "Texas")]
                expected' = Just (Section (Header "states") states)
            print m
            r' `shouldBe` expected'
    describe "INI parsing" $
        it "Can parse multiple sections" $ do
            let m = parseByteString parseIni mempty sectionEx''
                r' = maybeSuccess m
                sectionValues = M.fromList
                    [ ("alias", "claw"), ("host", "wikipedia.org")]
                whatisitValues = M.fromList [("red", "intoothandclaw")]
                expected' = Just (Config (M.fromList
                    [ (Header "section" , sectionValues)
                    , (Header "whatisit" , whatisitValues)]))
            print m
            r' `shouldBe` expected'
