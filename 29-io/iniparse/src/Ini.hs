{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE QuasiQuotes #-}

module Ini (parseIni, Config) where

-- INI format parsing

import qualified Data.Map as M
import Data.Map (Map)
import Control.Applicative
import Text.Trifecta
-- import Data.ByteString (ByteString)
-- import Text.RawString.QQ

-- headers

-- headerExample :: ByteString
-- headerExample = "[blah]"

newtype Header = Header String deriving (Eq, Ord, Show)

parseBracketPair :: Parser a -> Parser a
parseBracketPair p = char '[' *> p <* char ']'

parseHeader :: Parser Header
parseHeader = parseBracketPair (Header <$> some letter)

-- assignments

-- assignmentExample :: ByteString
-- assignmentExample = "woot=1"

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

-- commentExample :: ByteString
-- commentExample = "; Last modified 1 April 2001 by Jane Doe"

-- commentExample' :: ByteString
-- commentExample' = "; blah\n; woot\n \n; hah"

skipComments :: Parser ()
skipComments = skipMany $ do
    char ';' <|> char '#'
    skipMany (noneOf "\n")
    skipEOL

-- sections

-- sectionExample :: ByteString
-- sectionExample = "; ignore me\n[states]\nChris=Texas"

-- sectionExample' :: ByteString
-- sectionExample' = [r|
-- ; ignore me
-- [states]
-- Chris=Texas
-- |]

-- sectionExample'' :: ByteString
-- sectionExample'' = [r|
-- ; comment
-- [section]
-- host=wikipedia.org
-- alias=claw

-- [whatisit]
-- red=intoothandclaw
-- |]

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

-- maybeSuccess :: Result a -> Maybe a
-- maybeSuccess (Success a) = Just a
-- maybeSuccess _ = Nothing
