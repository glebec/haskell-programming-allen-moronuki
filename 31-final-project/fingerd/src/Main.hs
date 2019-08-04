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

import Database.SQLite.Simple hiding (bind, close)
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.Types

import Network.Socket hiding (recv)

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

returnUsers :: Connection -> Socket -> IO ()
returnUsers dbConn soc = do
    rows <- query_ dbConn allUsers
    let usernames = map username rows
        newlineSeparated = T.concat $ intersperse "\n" usernames
    sendAll soc (encodeUtf8 newlineSeparated)

formatUser :: User -> ByteString
formatUser (User _ username shell homeDir realName _) = BS.concat
    [ "Login: ", e username, "\t\t\t\t",
      "Name: ", e realName, "\n",
      "Directory: ", e homeDir, "\t\t\t",
      "Shell: ", e shell, "\n" ]
  where
    e = encodeUtf8

returnUser :: Connection -> Socket -> Text -> IO ()
returnUser dbConn soc username = do
    maybeUser <- getUser dbConn (T.strip username)
    case maybeUser of
        Nothing -> do
            putStrLn $ "Couldn't find matching user for username: " ++ show username
            sendAll soc "That user was not found."
        Just user -> sendAll soc (formatUser user)

handleQuery :: Connection -> Socket -> IO ()
handleQuery dbConn soc = do
    msg <- recv soc 1024
    case msg of
        "\n" -> returnUsers dbConn soc
        name -> returnUser dbConn soc (decodeUtf8 name)

handleQueries :: Connection -> Socket -> IO ()
handleQueries dbConn sock = forever $ do
    (soc, _) <- accept sock
    putStrLn "Got connection, handling query"
    handleQuery dbConn soc
    close soc

main :: IO ()
main = withSocketsDo $ do
    addrInfos <- getAddrInfo
                     (Just $ defaultHints {addrFlags = [AI_PASSIVE]})
                     Nothing
                     (Just "79")
    let serverAddr = head addrInfos
    sock <- socket (addrFamily serverAddr) Stream defaultProtocol
    bind sock (addrAddress serverAddr)
    listen sock 1  -- one connection at a time
    conn <- open "finger.db"
    handleQueries conn sock
    SQLite.close conn
    close sock
