{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Exception
import Data.Typeable

import System.Environment (getArgs)

import Data.Text (Text)

import Database.SQLite.Simple.Types (Null(..))
import Database.SQLite.Simple
    ( fromRow
    , toRow
    , FromRow
    , ToRow
    , Query
    , field
    , open
    , execute
    , query_
    )
import qualified Database.SQLite.Simple as SQLite

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

allUsers :: Query
allUsers = "SELECT * FROM users"

insertUser :: Query
insertUser = "INSERT INTO users VALUES (?, ?, ?, ?, ?, ?)"

data MissingData = MissingData deriving (Eq, Show, Typeable)
instance Exception MissingData

main :: IO ()
main = do
    args <- getArgs
    print args
    newRow <- case args of
        (f1:f2:f3:f4:f5:_) -> pure (Null, f1, f2, f3, f4, f5)
        _ -> throwIO MissingData
    conn <- open "finger.db"
    execute conn insertUser newRow
    rows <- query_ conn allUsers
    mapM_ print (rows :: [User])
    SQLite.close conn
