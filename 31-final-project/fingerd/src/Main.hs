{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Exception
import Control.Monad (forever)
import Data.List (intersperse)

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

import Data.Typeable

import Database.SQLite.Simple hiding (close)
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.Types

import Network.Socket hiding (close, recv)

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import Network.Socket.ByteString (recv, sendAll)

import Text.RawString.QQ

data User =
    User {
        userId :: Integer
      , username :: Text
      , shell :: Text
      , homeDirectory :: Text
      , realName :: Text
      , phone :: Text
    } deriving (Eq, Show)

instance FromRow User where
    fromRow = User <$> field <*> field <*> field <*> field <*> field <*> field

instance ToRow User where
    toRow User{..} = toRow (userId, username, shell, homeDirectory, realName, phone)

createUsers :: Query
createUsers = [r|
    CREATE TABLE IF NOT EXISTS users
    (id INTEGER PRIMARY KEY AUTOINCREMENT,
     username TEXT UNIQUE,
     shell TEXT,
     homeDirectory TEXT,
     realName TEXT,
     phone TEXT)
|]

insertUser :: Query
insertUser = "INSERT INTO users VALUES (?, ?, ?, ?, ?, ?)"

allUsers :: Query
allUsers = "SELECT * FROM users"

getUserQuery :: Query
getUserQuery = "SELECT * FROM users WHERE username = ?"

data DuplicateData = DuplicateData deriving (Eq, Show, Typeable)
instance Exception DuplicateData

type UserRow = (Null, Text, Text, Text, Text, Text)

getUser :: Connection -> Text -> IO (Maybe User)
getUser conn username = do
    results <- query conn getUserQuery (Only username)
    case results of
        [] -> pure Nothing
        [user] -> pure $ Just user
        _ -> throwIO DuplicateData

createDatabase :: IO ()
createDatabase = do
    conn <- open "finger.db"
    execute_ conn createUsers
    execute conn insertUser meRow
    rows <- query_ conn allUsers
    mapM_ print (rows :: [User])
    SQLite.close conn
  where
    meRow :: UserRow
    meRow = (Null, "glebec", "/bin/zsh", "/home/glebec", "G. Lebec", "555-123-4567")

main :: IO ()
main = createDatabase
