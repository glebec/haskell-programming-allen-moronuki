{-# LANGUAGE RecordWildCards #-}

module User (User(..), UserRow) where

import Data.Text (Text)
import Database.SQLite.Simple.Types (Null(..))
import Database.SQLite.Simple
    ( fromRow
    , toRow
    , FromRow
    , ToRow
    , field
    )

data User =
    User {
        userId :: Integer
      , username :: Text
      , shell :: Text
      , homeDirectory :: Text
      , realName :: Text
      , phone :: Text
    } deriving (Eq, Show)

type UserRow = (Null, Text, Text, Text, Text, Text)

instance FromRow User where
    fromRow = User <$> field <*> field <*> field <*> field <*> field <*> field

instance ToRow User where
    toRow User{..} = toRow (userId, username, shell, homeDirectory, realName, phone)
