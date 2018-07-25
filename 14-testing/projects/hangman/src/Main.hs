module Main where

import Control.Monad (forever, when)
import Data.Char (toLower)
import Data.Maybe (isJust, fromMaybe, catMaybes)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

minWordLength = 5 :: Int
maxWordLength = 9 :: Int
guessLimit = 7 :: Int

newtype WordList = WordList [String] deriving (Eq, Show)

allWords :: IO WordList
allWords = (WordList . lines) <$> readFile "data/dict.txt"
-- allWords = do
--     dict <- readFile "data/dict.txt"
--     return $ lines dict

gameWords :: IO WordList
gameWords = do
    (WordList aw) <- allWords
    return $ WordList (filter gameLength aw)
    where gameLength w = let l = length (w :: String)
                         in  l >= minWordLength &&
                             l <  maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList ws) = (ws !!) <$> randomRIO (0, length ws - 1)
-- randomWord ws = do
--     idx <- randomRIO (0, length ws - 1)
--     return $ ws !! idx

randomWord' = allWords >>= randomWord

data Puzzle = Puzzle String [Maybe Char] [Char] deriving Eq-- word, found, wrong guesses
instance Show Puzzle where
    show (Puzzle _ found guessed) =
        intersperse ' ' (fmap renderPuzzleChar found)
        ++ " Wrong guesses so far: " ++ guessed ++ " " ++ spellHangman guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar = fromMaybe '_'

freshPuzzle :: String -> Puzzle
freshPuzzle word = Puzzle word (fmap (const Nothing) word) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) c = c `elem` word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guesses) c = c `elem` guesses

alreadyFound :: Puzzle -> Char -> Bool
alreadyFound (Puzzle _ found _) c = c `elem` catMaybes found

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter p@(Puzzle word found gs) g =
    Puzzle word found' (if charInWord p g then gs else g : gs)
    where found' = zipWith (match g) word found
          match guessed wordChar guessChar =
              if wordChar == guessed
              then Just wordChar
              else guessChar

spellHangman :: String -> String
spellHangman guesses = take (length guesses) "HANGMAN"

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
    putStrLn $ "Your guess was: " ++ [guess]
    case ( charInWord puzzle guess
         , alreadyGuessed puzzle guess
         , alreadyFound puzzle guess) of
        (_, _, True) -> do
            putStrLn "Already found, please try something else."
            return puzzle
        (_, True, _) -> do
            putStrLn "You already tried that, please try something else."
            return puzzle
        (True, _, _) -> do
            putStrLn "Good guess! Filling in the word."
            return (fillInCharacter puzzle guess)
        (False, _, _) -> do
            putStrLn "Sorry, that's not in the word."
            return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle word _ gs) =
    when (length gs >= guessLimit) $ do
        putStrLn "Sorry, game over."
        putStrLn $ "The word was " ++ word
        exitSuccess
    -- if length gs > 7 then
    --     do putStrLn "Sorry, game over."
    --        putStrLn $ "The word was " ++ word
    --        exitSuccess
    -- else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle word found _) =
    when (all isJust found) $ do
        putStrLn "Congratulations!"
        putStrLn $ "The word was " ++ word
        exitSuccess

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
    gameOver puzzle
    gameWin puzzle
    putStrLn $
        "Current puzzle is: " ++ show puzzle
    putStr "Guess a letter: "
    guess <- getLine
    case guess of
        [c] -> handleGuess puzzle c >>= runGame
        _   -> putStrLn "Your guess must be a single character."

main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
