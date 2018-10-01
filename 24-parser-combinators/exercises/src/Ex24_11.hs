module Ex24_11 where

import Control.Applicative
import Text.Trifecta
import Text.Parser.LookAhead

-- 1. Write a parser for semver as per http://semver.org. Then write an `Ord`
-- instance for `SemVer` that obeys the spec on the site.

data NumberOrString =
      NOSS String
    | NOSI Integer
    deriving (Eq, Ord, Show) -- N.B. that NOSS > NOSI by this derivation

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer = SemVer Major Minor Patch Release Metadata deriving (Eq, Show)

instance Ord SemVer where
    compare (SemVer mj mn pt rl me) (SemVer mj' mn' pt' rl' me')
        | (mj, mn, pt, rl) >  (mj', mn', pt', rl') = GT
        | (mj, mn, pt, rl) <  (mj', mn', pt', rl') = LT
        | otherwise = EQ

parseVersion :: Parser (Major, Minor, Patch)
parseVersion = do
    maj <- decimal
    char '.'
    min <- decimal
    char '.'
    ptc <- decimal
    return (maj, min, ptc)

alphaNums :: String
alphaNums = ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z'] ++ "-"

parseNOSS :: Parser NumberOrString
parseNOSS = NOSS <$> some (oneOf alphaNums)

parseNOSI :: Parser NumberOrString
parseNOSI = NOSI <$> decimal

parseMetaIdentifier :: Parser NumberOrString
parseMetaIdentifier = parseNOSS -- <|> parseNOSI -- removed b/c wrong per spec

parseRelIdentifier :: Parser NumberOrString
parseRelIdentifier = (lookAhead (noneOf "0") >> parseNOSI) <|>
                     parseNOSS

parseRelease :: Parser Release
parseRelease = do
    char '-'
    sepBy1 parseRelIdentifier (char '.')

parseMetadata :: Parser Metadata
parseMetadata = do
    char '+'
    sepBy1 parseMetaIdentifier (char '.')

parseSemVer :: Parser SemVer
parseSemVer = do
    (maj, min, ptc) <- parseVersion
    rel  <- try parseRelease  <|> pure []
    meta <- try parseMetadata <|> pure []
    return $ SemVer maj min ptc rel meta

demoParseSemVer :: String -> Result SemVer
demoParseSemVer = parseString parseSemVer mempty

sv1, sv2, sv3 :: Result SemVer
sv1 = demoParseSemVer "2.1.1"
sv2 = demoParseSemVer "1.0.0-x.7.z.92"
sv3 = demoParseSemVer "19.2.38-alpha.01+myver2.09"

svComp1, svComp2 :: Bool
svComp1 = SemVer 2  1 1 [] []  > SemVer 2 1 0 [] []
svComp2 = SemVer 12 3 1 [] []  > SemVer 2 1 0 [] []

svComp3 :: Result Bool
svComp3 = (>) <$> demoParseSemVer "19.2.38-alpha.10+myver3.09" <*>
                  demoParseSemVer "19.2.38-alpha.4+myver2.09"
