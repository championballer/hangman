module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

type WordList = [String]

allWords :: WordList
allWords = do
 dict <- readFile "data/dict.txt"
 return (lines dict)

minWordLength :: Int 
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: WordList
gameWords = do
 aw <- allWords
 return (filter gameLength aw)
 where gameLength w = 
 	let l = length (w::String)
 	in      l >= minWordLength
 	    &&  l <  maxWordLength

randomWord :: WordList -> IO String
randomWord wl = do
 randomIndex = randomRIO (0, length wl-1)
 return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
 show (Puzzle _ discovered guessed) = 
 	(intersperse ' ' $ fmap renderPuzzleChar discovered)
 	++ " Guessed so far " ++ guessed

f = const Nothing

freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle (s (fmap f s) [])

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle s _ _) c = elem c s

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ g) c = elem c g
 
renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c

-- Handles the case wherein the char matches with multiple entities in the same string

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c = 
 Puzzle word newFilledSoFar (c:s)
  where zipper guessed wordChar guessChar = 
  	if wordChar == guessed
  	then Just wordChar
  	else guessChar
  newFilledSoFar = zipWith (zipper c) word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
 putStrLn $ "Your guess was: " ++ [guess]
 case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
 	(_,True) -> do
 		putStrLn "You already guessed that, say something new"

main :: IO ()
main = do
  putStrLn "hello world"