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

{-|
This function handles the puzzle and the guess, when we know that it's a new guess. We try to flip over all nothings where this char can be put,
So for the newFilledSoFar, the guessed character is checked with the word string, if they match then the card gets flipped, else we return the value as was
present in the previous state maintained in filledSoFar, sent to the inscope argument guessChar.
-}

--TODO : Improve the name for guessChar, to identify the position of filledSoFar, not so clear right now.

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c = 
 Puzzle word newFilledSoFar (c:s)
  where zipper guessed wordChar guessChar = 
  	if wordChar == guessed
  	then Just wordChar
  	else guessChar
  newFilledSoFar = zipWith (zipper c) word filledInSoFar


{- |
This function accepts the current puzzle state and the guessed char, the guessed char is sent to the the two boolean functions to get
the state of presence of the char, if already guessed, we go back. If present, we turn over all the positions where the character was present
along with adding it into already made guesses section, and if it isn't still, goes in the made guesses section.
-}
handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
 putStrLn $ "Your guess was: " ++ [guess]
 case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
 	(_,True) -> do
 		putStrLn "You already guessed that, say something new"
 		return puzzle
 	(True,_) -> do
 		putStrLn "You found a letter of the word!"
 		return (fillInCharacter puzzle guess)
 	(False,_) -> do
 		putStrLn "You chose a word that isn't in the word"
 		return (fillInCharacter puzzle games)

gameOver :: Puzzle -> IO()
gameOver (Puzzle wordToGuess _ guessed) = 
	if (length guessed) > 7 then
		do putStrLn "You lose!"
		   putStrLn $ "The word was " ++ wordToGuess
		   exitSuccess
    else return ()

gameWin :: Puzzle -> IO()
gameWin (Puzzle _ filledInSoFar _) = 
	if all isJust filledInSoFar then
		do putStrLn "You won"
		exitSuccess
	else return ()

runGame :: Puzzle -> IO()
runGame puzzle = forever $ do
	gameOver puzzle
	gameWin puzzle
	putStrLn $ "Current puzzle is:" ++ show puzzle
	putStrLn "Guess a letter:"
	guess <- getLine
	case guess of 
		[c] -> handleGuess puzzle c >>= runGame
		_ -> putStrLn "Your guess must be a single char"

main :: IO ()
main = do
 word <- randomWord'
 let puzzle = freshPuzzle (fmap toLower word)
 runGame puzzle