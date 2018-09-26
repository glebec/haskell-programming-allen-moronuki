module Ex21_8_Wreq where

import qualified Data.ByteString.Lazy as B
import Network.Wreq
import Control.Lens

urls :: [String]
urls = [ "https://jsonplaceholder.typicode.com/posts/9"
       , "https://jsonplaceholder.typicode.com/users/4"
       ]

mappingGet :: [IO (Response B.ByteString)]
mappingGet = map get urls

getUrls :: IO [Response B.ByteString]
getUrls = sequence mappingGet

printRespBodies :: IO ()
printRespBodies = do
    resps <- getUrls
    print $ (^. responseBody) <$> resps
