{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception
import Data.Typeable

import System.Environment (getArgs)

import Database.SQLite.Simple.Types (Null(..))
import Database.SQLite.Simple
    ( Query
    , open
    , execute
    , query_
    , close
    )

import User (User)

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
    close conn
