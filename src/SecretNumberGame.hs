module SecretNumberGame where

import System.Random
import System.Console.ANSI (clearScreen)
import Text.Read (readMaybe)

type GuessRange = (Integer, Integer)
type Answer     = Integer
type Guess      = Integer

data GameState      =  GameState
    { numberToGuess :: Answer
    , numTries      :: Integer
    , guessRange    :: GuessRange
    } deriving (Show)

minGuess = 1
maxGuess = 100

calculateNewRange :: GuessRange -> Answer -> Guess -> GuessRange
calculateNewRange range answer guess
    | guess < answer = (guess, snd range)
    | otherwise = (fst range, guess)

correctGuessText :: GameState -> String
correctGuessText gs = 
       show (numberToGuess gs) 
    ++ " is correct! It took you " 
    ++ show (numTries gs)
    ++ tryOrTries (numTries gs)
        where tryOrTries n = if n == 1 then " try." else " tries."

validGuess :: String -> Bool
validGuess s =
    case readMaybe s :: Maybe Integer of
        Nothing -> False
        Just s  -> True

-- not using >= or <= because for some reason it didnt pass the tests. 
guessIsInRange :: GuessRange -> Integer -> Bool
guessIsInRange range guess
    | guess <  fst range = False
    | guess >  snd range = False
    | guess == fst range = False
    | guess == snd range = False
    | otherwise          = True

-- TODO: Dont nest ifs
safeRead :: GameState -> IO Integer
safeRead gs = do
    s <- getLine
    if validGuess s
        then
            if guessIsInRange (guessRange gs) (read s)
                then return $ read s
                else safeRead gs
        else putStrLn "Only integers" >> safeRead gs

tooLow :: Integer -> Integer -> Bool
tooLow guess answer = guess < answer

tooLowOrTooHighText :: Integer -> Integer -> String
tooLowOrTooHighText answer guess =
    if answer > guess 
        then "Too low" 
        else "Too high"

gameLoop :: GameState -> IO ()
gameLoop gs = do
    print $ numberToGuess gs
    putStrLn "Enter a number:"
    s <- safeRead gs
    let num = s :: Integer
    if num == numberToGuess gs
        then putStrLn $ correctGuessText gs
        else do
            putStrLn $ tooLowOrTooHighText (numberToGuess gs) s
            gameLoop $ GameState (numberToGuess gs) (numTries gs + 1) (minGuess, maxGuess)

initGame :: IO ()
initGame = do
    answer <- randomRIO (minGuess, maxGuess)
    let gameState = GameState answer 1 (minGuess, maxGuess)
    clearScreen
    putStrLn $ howToPlayText minGuess maxGuess
    gameLoop gameState
        where howToPlayText min max = "Enter a number between " ++ show min ++ " and " ++ show max

