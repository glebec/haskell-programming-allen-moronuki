module Ex24_11_4 where

import Control.Applicative
import Text.Trifecta

-- 4. Phone nums

-- There are a ton of directions we could take this. The solution below seeks
-- only to replicate exactly the behavior demonstrated in the book.

type NumPlanArea = Int
type Exchange = Int
type LineNum = Int

data PhoneNumber = PhoneNumber NumPlanArea Exchange LineNum deriving (Eq, Show)

trunk :: Parser ()
trunk = string "1-" >> pure ()

areaNum :: Parser Int
areaNum = read <$> count 3 digit

exchNum :: Parser Int
exchNum = areaNum

lineNum :: Parser Int
lineNum = read <$> count 4 digit

dashedExchLine :: Parser (Int, Int)
dashedExchLine = do
    e <- exchNum
    char '-'
    l <- lineNum
    pure (e, l)

concisePhone :: Parser PhoneNumber
concisePhone = PhoneNumber <$> areaNum <*> exchNum <*> lineNum

parenPhone :: Parser PhoneNumber
parenPhone = do
    try (char '(')
    a <- areaNum
    string ") "
    (e, l) <- dashedExchLine
    pure $ PhoneNumber a e l

dashedPhone :: Parser PhoneNumber
dashedPhone = do
    a <- try (areaNum <* char '-')
    (e, l) <- dashedExchLine
    pure $ PhoneNumber a e l

fullPhone :: Parser PhoneNumber
fullPhone = try trunk >> dashedPhone

phone :: Parser PhoneNumber
phone = fullPhone <|> parenPhone <|> dashedPhone <|> concisePhone
