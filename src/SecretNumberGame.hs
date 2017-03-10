module SecretNumberGame
    ( initGame
    , tooLowOrTooHighText
    , correctGuessText
    , validGuess
    , GameState (GameState)
    ) where

import System.Random
import System.Console.ANSI (clearScreen)
import Text.Read (readMaybe)

data GameState = GameState
    { numberToGuess :: Integer
    , numTries      :: Integer
    } deriving (Show)

minGuess = 1
maxGuess = 100

correctGuessText :: GameState -> String
correctGuessText gs = 
    show (numberToGuess gs) ++ 
    " is correct! It took you " ++ 
    show (numTries gs) ++ 
    tryOrTries (numTries gs)
        where tryOrTries n = if n == 1 then " try." else " tries."

validGuess :: String -> Bool
validGuess s =
    case readMaybe s :: Maybe Integer of
        Nothing -> False
        Just s  -> True

safeRead :: IO Integer
safeRead = do
    s <- getLine
    if validGuess s
        then return $ read s
        else putStrLn "Only integers" >> safeRead

tooLowOrTooHighText :: Integer -> Integer -> String
tooLowOrTooHighText answer guess =
    if answer > guess 
        then "Too low" 
        else "Too high"

gameLoop :: GameState -> IO ()
gameLoop gs = do
    print $ numberToGuess gs
    putStrLn "Enter a number:"
    s <- safeRead
    let num = s :: Integer
    if num == numberToGuess gs
        then putStrLn $ correctGuessText gs
        else do
            putStrLn $ tooLowOrTooHighText (numberToGuess gs) s
            gameLoop $ GameState (numberToGuess gs) (numTries gs + 1)

initGame :: IO ()
initGame = do
    answer <- randomRIO (minGuess, maxGuess)
    let gameState = GameState answer 1
    clearScreen
    putStrLn $ howToPlayText minGuess maxGuess
    gameLoop gameState
        where howToPlayText min max = "Enter a number between " ++ show min ++ " and " ++ show max

