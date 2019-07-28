module Main where

import System.Exit (die)
import System.Environment (getArgs)
import System.FilePath (combine, takeExtension)
import System.Directory (listDirectory, makeAbsolute)
import Text.Trifecta (Result, parseString)
import qualified Data.Map as M
import Ini (parseIni, Config)

ini :: String -> Result Config
ini = parseString parseIni mempty

main :: IO ()
main = do
    args <- getArgs
    path <- case args of
        [] -> die "Missing path argument"
        (p:_) -> makeAbsolute p
    putStrLn path
    fileNames <- listDirectory path
    let iniFileNames = filter ((== ".ini") . takeExtension) fileNames
        absFileNames = combine path <$> iniFileNames
    files <- traverse readFile absFileNames
    let result = M.fromList $ zip fileNames (ini <$> files)
    print result
