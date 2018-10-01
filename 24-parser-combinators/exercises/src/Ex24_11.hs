module Ex24_11 where

import Control.Applicative
import Text.Trifecta
import Text.Parser.LookAhead
import Test.Hspec
import Data.Foldable (sequenceA_)

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
        | (mj, mn, pt, rl) > (mj', mn', pt', rl') = GT
        | s == s'                                 = EQ
        | otherwise                               = LT

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
parseRelease = sepBy1 parseRelIdentifier (char '.')

parseMetadata :: Parser Metadata
parseMetadata = sepBy1 parseMetaIdentifier (char '.')

parseSemVer :: Parser SemVer
parseSemVer = do
    (maj, min, ptc) <- parseVersion
    rel  <- option [] (char '-' >> parseRelease)
    meta <- option [] (char '+' >> parseMetadata)
    return $ SemVer maj min ptc rel meta

psv :: String -> Result SemVer
psv = parseString parseSemVer mempty

-- Testing

yields :: Eq a => a -> Result a -> Bool
yields s (Success s') = s == s'
yields _ _ = False

pFailed :: Result a -> Bool
pFailed (Success _) = False
pFailed _ = True

specParseYields :: String -> SemVer -> SpecWith ()
specParseYields s sv =
    it ("parses " ++ s ++ " as " ++ show sv) $
        psv s `shouldSatisfy` yields sv

specParseFails :: String -> SpecWith ()
specParseFails s =
    it ("fails to parse " ++ s) $
        psv s `shouldSatisfy` pFailed

specOp :: (SemVer -> SemVer -> Bool) -> String
       -> String -> String -> SpecWith ()
specOp op msg a b = it (a ++ " " ++ msg ++ " " ++ b) $
    liftA2 op (psv a) (psv b) `shouldSatisfy` yields True

specEQ, specLT :: String -> String -> SpecWith ()
specEQ = specOp (==) "equals"
specLT = specOp (<) "is less than"

checkSemVer :: IO ()
checkSemVer = hspec $ do
    describe "parseSemVer" $ do
        mapM_ (uncurry specParseYields)
            [ ( "0.0.0",
                SemVer 0 0 0 [] [] )
            , ( "2.1.3",
                SemVer 2 1 3 [] [] )
            , ( "14.103.9",
                SemVer 14 103 9 [] [] )
            , ( "1.1.1-alpha",
                SemVer 1 1 1 [NOSS "alpha"] [] )
            , ( "1.1.1-the-coolest.1",
                SemVer 1 1 1 [NOSS "the-coolest", NOSI 1] [] )
            , ( "1.2.3-x.8.wow.19"
              , SemVer 1 2 3 [NOSS "x", NOSI 8, NOSS "wow", NOSI 19] [] )
            , ( "0.1.0+mydata",
                SemVer 0 1 0 [] [NOSS "mydata"] )
            , ( "0.1.0+5.my-data.000",
                SemVer 0 1 0 [] [NOSS "5", NOSS "my-data", NOSS "000"] )
            , ( "1.2.3-13.0.beta+xo.08.0.wow"
              , SemVer 1 2 3
                [NOSI 13, NOSI 0, NOSS "beta"]
                [NOSS "xo", NOSS "08", NOSS "0", NOSS "wow"] ) ]
        mapM_ specParseFails
            [ "00.0.0", "0.00.0", "0.0.00"
            , "01.0.0", "0.01.0", "0.0.01"
            , "0.0.0-00", "0.0.0-x.00", "0.0.0-01.x"
            , "1", "1.2", "a", "1.a", "1.2.a"
            , "1.1.1-", "1.1.1+", "1.1.1-+", "1.1.1-+alpha" ]
    describe "SemVer Ord" $
        sequenceA_
            [ "1.0.0" `specEQ` "1.0.0"
            , "0.1.9" `specEQ` "0.1.9"
            , "0.1.9-alpha" `specEQ` "0.1.9-alpha"
            , "0.1.9+foo" `specEQ` "0.1.9+bar"
            , "0.1.9-alpha+foo" `specEQ` "0.1.9-alpha+bar"
            , "3.0.0" `specLT` "3.0.1"
            , "3.0.0" `specLT` "13.0.0"
            , "13.9.0" `specLT` "13.39.0"
            , "13.9.45" `specLT` "13.9.55"
            , "1.0.0-alpha" `specLT` "1.0.0-alpha.1"
            , "1.0.0-alpha.1" `specLT` "1.0.0-alpha.beta"
            , "1.0.0-beta" `specLT` "1.0.0-beta.2"
            , "1.0.0-beta" `specLT` "1.0.0-beta.2"
            , "1.0.0-beta.2" `specLT` "1.0.0-beta.11"
            , "1.0.0-beta.11" `specLT` "1.0.0-rc.1"
            , "1.0.0-rc.1" `specLT` "1.0.0"
            , "1.0.0-rc.1+102" `specLT` "1.0.0"
            , "1.0.0-rc.1+102" `specLT` "1.0.0+102" ]
