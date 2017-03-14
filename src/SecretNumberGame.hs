module SecretNumberGame where

import System.Random
import System.Console.ANSI
import Text.Read (readMaybe)
import Data.Maybe (isJust)
import Control.Monad (unless)

type GuessRange = (Integer, Integer)
type Answer     = Integer
type Guess      = (Integer, GuessState)

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
    | snd guess == Low = (succ $ fst guess, snd range)
    | otherwise        = (fst range, pred $ fst guess)

correctGuessText :: GameState -> String
correctGuessText gs = 
       show (numberToGuess gs) 
    ++ " is correct! It took you " 
    ++ show (numTries gs)
    ++ tryOrTries (numTries gs)
        where tryOrTries n = if n == 1 then " try." else " tries."

validGuess :: String -> Bool
validGuess s = isJust (readMaybe s :: Maybe Integer)

-- -- not using >= or <= because for some reason it didnt pass the tests. 
guessIsInRange :: GuessRange -> Integer -> Bool
guessIsInRange range guess
    | guess <  fst range = False
    | guess >  snd range = False
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

tooLowOrTooHighText :: Guess -> String
tooLowOrTooHighText guess =
    if snd guess == Low 
        then "Too low" 
        else "Too high"

renderStatus :: GameState -> IO ()
renderStatus state = do
    clearScreen
    if not $ null $ previousGuesses state 
        then do
            setSGR [SetColor Foreground Vivid Red]
            if lastGuessState state == Low
                then setSGR [SetColor Foreground Dull Red]  >> putStrLn "Too low" 
                else setSGR [SetColor Foreground Vivid Red] >> putStrLn "Too high"
        else putStrLn ""
    setSGR [Reset]
    setSGR [SetColor Foreground Dull White]
    putStrLn $ guessRangeText (guessRange state)
    putStrLn $ previousGuessesText (previousGuesses state) 
    setSGR [Reset]
    putStrLn "Enter a number:"

guessStatus :: Integer -> Integer -> Guess
guessStatus guess answer
    | guess < answer  = (guess, Low)
    | guess > answer  = (guess, High)
    | guess == answer = (guess, Correct)

updateGuessRangeInState :: GameState -> GuessRange -> GameState
updateGuessRangeInState gs rn = GameState (numberToGuess gs)
                                          (numTries gs)
                                          rn
                                          (previousGuesses gs)
                                          (lastGuessState gs)

gameLoop :: GameState -> IO ()
gameLoop gs = do
    renderStatus gs
    s <- safeRead gs
    let guessInt = s :: Integer
    let guess = guessStatus guessInt (numberToGuess gs)
    let newState = updateGuessRangeInState gs (calculateNewRange (guessRange gs) (numberToGuess gs) guess)
    let state = GameState 23 1 (1, 100) [(1, Low), (2, Low)] Low
    if snd guess == Correct
        then do
            clearScreen
            setSGR [SetColor Foreground Vivid Green]
            putStrLn $ correctGuessText newState
            setSGR [Reset]
        else do 
            putStrLn $ tooLowOrTooHighText guess
            gameLoop $ GameState (numberToGuess newState) 
                                 (numTries newState + 1) 
                                 (guessRange newState)
                                 (guess:previousGuesses gs)
                                 (snd guess)

initGame :: IO ()
initGame = do
    answer <- randomRIO (minGuess, maxGuess)
    let gameState = GameState answer 1 (minGuess, maxGuess) [] None
    gameLoop gameState

