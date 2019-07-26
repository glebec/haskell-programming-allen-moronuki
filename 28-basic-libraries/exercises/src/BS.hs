{-# LANGUAGE OverloadedStrings #-}

module BS (demoBS) where

import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import qualified Codec.Compression.GZip as GZip

input :: BL.ByteString
input = "123"

compressed :: BL.ByteString
compressed = GZip.compress input

demoBS :: IO ()
demoBS = do
    TIO.putStrLn $ TE.decodeUtf8 (s input)
    TIO.putStrLn $ TE.decodeUtf8 (s $ GZip.decompress compressed)
    TIO.putStrLn $ TE.decodeUtf8 (s compressed) -- fails (as intended for demo)
    where s = BL.toStrict
