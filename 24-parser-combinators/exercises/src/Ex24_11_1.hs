module Ex24_11_1 where

import Control.Applicative
import Text.Trifecta
import Test.Hspec
import Data.Foldable (sequenceA_)
import Data.Function (on)
import Data.Ord (comparing)

-- 1. Write a parser for semver as per http://semver.org. Then write an `Ord`
-- instance for `SemVer` that obeys the spec on the site.

data NumberOrString =
      NOSI Integer
    | NOSS String
    deriving (Eq, Ord, Show) -- N.B. that NOSS > NOSI by this derivation

type Major = Integer
type Minor = Integer
type Patch = Integer
type PreRelease = [NumberOrString]
type Metadata = [String]

data SemVer = SemVer Major Minor Patch PreRelease Metadata deriving (Eq, Show)

-- Making an `instance Ord SemVer` either means having an `Eq` instance which
-- ignores metadata, or having `Ord` & `Eq` disagree. The solution here uses
-- a newtype for precedence comparison; thanks to Christoph Horst for the idea.
newtype SemVerPrecedence = SemVerPrecedence SemVer deriving (Show)

precAsTuple :: SemVerPrecedence -> (Major, Minor, Patch, Bool, PreRelease)
precAsTuple (SemVerPrecedence (SemVer mj  mn  pt  rl  _)) =
    -- we give earlier precedence to semver *without* pre-release identifiers
    (mj, mn, pt, null rl, rl)

instance Eq SemVerPrecedence where
    (==) = (==) `on` precAsTuple

instance Ord SemVerPrecedence where
    compare = comparing precAsTuple

-- parsing

{-
Official BNF at https://github.com/semver/semver/blob/master/semver.md#backusnaur-form-grammar-for-valid-semver-versions
EBNF below adapted from https://github.com/semver/semver.org/issues/59#issuecomment-393560776

Version ::= VersionCore ('-' PreRelease)? ('+' Meta)?

VersionCore ::= Major '.' Minor '.' Patch
Major ::= Numeric
Minor ::= Numeric
Patch ::= Numeric
PreRelease ::= PreReleaseId ('.' PreReleaseId)*
Meta       ::= MetaId ('.' MetaId)*

PreReleaseId ::= AlphaNums | Numeric
MetaId       ::= IdChar+

Numeric   ::= '0' | ( PosNum Digit* )
AlphaNums ::= Digit* NonNum IdChar*

IdChar ::= NonNum | Digit

NonNum ::= [A-Za-z-]
Digit  ::= '0' | PosNum
PosNum ::= [1-9]
-}

posNum :: Parser Char
posNum = oneOf ['1'..'9']

nonNum :: Parser Char
nonNum = letter <|> char '-'

idChar :: Parser Char
idChar = nonNum <|> digit

numeric :: Parser Integer
numeric = (string "0" >> pure 0) <|>
          (do d  <- posNum
              ds <- many digit
              pure $ read (d:ds))

alphaNums :: Parser String
alphaNums = do
    cs1 <- many digit
    c   <- nonNum
    cs2 <- many idChar
    pure $ cs1 ++ [c] ++ cs2

metaId :: Parser String
metaId = some idChar

preReleaseId :: Parser NumberOrString
preReleaseId = (NOSS <$> try alphaNums) <|>
               (NOSI <$> try numeric)

preRelease :: Parser PreRelease
preRelease = sepBy1 preReleaseId (char '.')

meta :: Parser Metadata
meta = sepBy1 metaId (char '.')

version :: Parser (Major, Minor, Patch)
version = do
    maj <- numeric
    char '.'
    min <- numeric
    char '.'
    ptc <- numeric
    pure (maj, min, ptc)

parseSemVer :: Parser SemVer
parseSemVer = do
    (maj, min, ptc) <- version
    pre <- option [] (char '-' >> preRelease)
    met <- option [] (char '+' >> meta)
    pure $ SemVer maj min ptc pre met

-- Testing

-- using `eof` for cleaner testing – enforcing entire string is relevant
psv :: String -> Result SemVer
psv = parseString (parseSemVer <* eof) mempty

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
specEQ = specOp ((==) `on` SemVerPrecedence) "equals"
specLT = specOp ((<)  `on` SemVerPrecedence) "is less than"

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
                SemVer 0 1 0 [] ["mydata"] )
            , ( "0.1.0+5.my-data.000",
                SemVer 0 1 0 [] ["5", "my-data", "000"] )
            , ( "1.2.3-13.0.beta+xo.08.0.wow"
              , SemVer 1 2 3
                [NOSI 13, NOSI 0, NOSS "beta"]
                ["xo", "08", "0", "wow"] ) ]
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
