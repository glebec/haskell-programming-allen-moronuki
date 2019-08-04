module Write where

import Control.Exception
-- import Data.Typeable (typeOf)

-- handler :: SomeException -> IO ()
-- handler (SomeException e) = do
--     print (typeOf e)
--     putStrLn $ "Error: " ++ show e

handler :: SomeException -> IO ()
handler (SomeException e) = do
    putStrLn $ "Error: " ++ show e
    writeFile "bbb" "hi"

main :: IO ()
main = do
    -- writeFile "aaa" "hi"
    writeFile "zzz" "hi"
        `catch` handler
    putStrLn "program completing"
