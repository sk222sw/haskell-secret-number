module SecretNumberGame where

import System.Random
import System.Console.ANSI (clearScreen)
import Text.Read (readMaybe)
import Data.Maybe (isJust)
import Control.Monad (unless)

type GuessRange = (Integer, Integer)
type Answer     = Integer
type Guess      = Integer

type Low = String
type Hight = String
type None = String

data GuessState = Low | High | Correct | None deriving (Show, Ord, Eq)

data GameState = GameState
    { numberToGuess   :: Answer
    , numTries        :: Integer
    , guessRange      :: GuessRange
    , previousGuesses :: [Guess]
    , lastGuessState  :: GuessState
    } deriving (Show, Eq)

minGuess = 1
maxGuess = 100

guessRangeText :: GuessRange -> String
guessRangeText range = "Valid guesses: " ++ show (fst range) ++ " to " ++ show (snd range)

previousGuessesText :: [Guess] -> String
previousGuessesText xs = init $ "Previous guesses: " ++ guessesToString xs
    where guessesToString [] = ""
          guessesToString (x:xs) = show x ++ " " ++ guessesToString xs

calculateNewRange :: GuessRange -> Answer -> Guess -> GuessRange
calculateNewRange range answer guess
    | guess < answer = (guess, snd range)
    | otherwise      = (fst range, guess)

correctGuessText :: GameState -> String
correctGuessText gs = 
       show (numberToGuess gs) 
    ++ " is correct! It took you " 
    ++ show (numTries gs)
    ++ tryOrTries (numTries gs)
        where tryOrTries n = if n == 1 then " try." else " tries."

validGuess :: String -> Bool
validGuess s = isJust (readMaybe s :: Maybe Integer)

-- not using >= or <= because for some reason it didnt pass the tests. 
guessIsInRange :: GuessRange -> Integer -> Bool
guessIsInRange range guess
    | guess <  fst range = False
    | guess >  snd range = False
    | guess == fst range = False
    | guess == snd range = False
    | otherwise          = True

outOfRangeText :: GuessRange -> String
outOfRangeText range = 
        "Your guess should be between " 
     ++ show (fst range) 
     ++ " and "
     ++ show (snd range)

safeRead :: GameState -> IO Integer
safeRead gs = do
    s <- getLine
    if validGuess s
        then if guessIsInRange (guessRange gs) (read s)
                then return $ read s
                else putStrLn (outOfRangeText (guessRange gs)) >> safeRead gs
        else putStrLn "Only integers" >> safeRead gs

tooLowOrTooHighText :: Integer -> Integer -> String
tooLowOrTooHighText answer guess =
    if answer > guess 
        then "Too low" 
        else "Too high"

renderStatus :: GameState -> IO ()
renderStatus state = do
    clearScreen
    putStrLn $ guessRangeText (guessRange state)
    putStrLn $ previousGuessesText (previousGuesses state) 
    putStrLn "Enter a number:"

guessStatus :: Guess -> Integer -> GuessState
guessStatus guess answer
    | guess < answer  = Low
    | guess > answer  = High
    | guess == answer = Correct

updateGuessRangeInState :: GameState -> GuessRange -> GameState
updateGuessRangeInState gs rn = GameState (numberToGuess gs)
                                          (numTries gs)
                                          rn
                                          (previousGuesses gs)
                                          (lastGuessState gs)

gameLoop :: GameState -> IO ()
gameLoop gs = do
    print $ numberToGuess gs
    renderStatus gs
    s <- safeRead gs
    let guess = s :: Integer
    let newState = updateGuessRangeInState gs (calculateNewRange (guessRange gs) (numberToGuess gs) s)
    if guessStatus guess (numberToGuess newState) == Correct
        then do
            clearScreen
            putStrLn $ correctGuessText newState
        else do 
            putStrLn $ tooLowOrTooHighText (numberToGuess gs) s
            gameLoop $ GameState (numberToGuess newState) 
                                 (numTries newState + 1) 
                                 (guessRange newState)
                                 (s:previousGuesses gs)
                                 (guessStatus s (numberToGuess gs))

initGame :: IO ()
initGame = do
    answer <- randomRIO (minGuess, maxGuess)
    let gameState = GameState answer 1 (minGuess, maxGuess) [] None
    gameLoop gameState

