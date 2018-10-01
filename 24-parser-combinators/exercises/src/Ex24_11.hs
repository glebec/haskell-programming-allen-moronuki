module Ex24_11 where

import Control.Applicative
import Text.Trifecta
import Text.Parser.LookAhead

-- 1. Write a parser for semver as per http://semver.org. Then write an `Ord`
-- instance for `SemVer` that obeys the spec on the site.

data NumberOrString =
      NOSI Integer
    | NOSS String
    deriving (Eq, Ord, Show) -- N.B. that NOSS > NOSI by this derivation

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer = SemVer Major Minor Patch Release Metadata deriving (Show)

instance Eq SemVer where
    (==) (SemVer mj mn pt rl _) (SemVer mj' mn' pt' rl' _) =
        (mj, mn, pt, rl) == (mj', mn', pt', rl')

instance Ord SemVer where
    compare s@(SemVer mj mn pt rl _) s'@(SemVer mj' mn' pt' rl' _)
        | s == s' = EQ
        | otherwise = if (mj, mn, pt, rl) > (mj', mn', pt', rl')
                      then GT
                      else LT

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
svComp3 = SemVer 1  1 1 [NOSS "alpha"] [] > SemVer 1  1 1 [NOSI 1] []

svComp4 :: Result Bool
svComp4 = (>) <$> demoParseSemVer "19.2.38-alpha.10+myver3.09" <*>
                  demoParseSemVer "19.2.38-alpha.4+myver2.09"
