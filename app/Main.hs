{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Random
import System.Console.ANSI (clearScreen)
import Text.Read (readMaybe)

data GameState = GameState
    { numberToGuess::Integer
    , numTries     ::Integer
    } deriving (Show)

-- type Guess = Integer

minGuess = 1
maxGuess = 4

showCorrectText :: GameState -> IO ()
showCorrectText gs = clearScreen >> putStrLn "That was correct!"

howToPlayText :: Integer -> Integer -> String
howToPlayText min max = "Enter a number between " ++ show min ++ " and " ++ show max

safeRead :: IO Integer
safeRead = do
    s <- getLine
    case readMaybe s :: Maybe Integer of
        Nothing -> putStrLn "Only numbers" >> safeRead
        Just s  -> return s

tooLowOrTooHighText :: Integer -> Integer -> String
tooLowOrTooHighText answer guess =
    if answer > guess 
        then "Too low" 
        else "Too high"

gameLoop :: GameState -> IO ()
gameLoop gs = do
    putStrLn "Enter a number:"
    s <- safeRead
    let num = s :: Integer
    if num == numberToGuess gs
        then showCorrectText gs
        else do
            putStrLn $ tooLowOrTooHighText (numberToGuess gs) s
            gameLoop $ GameState (numberToGuess gs) (numTries gs + 1)

main = do
    pTime <- randomRIO (minGuess, maxGuess)
    let gameState = GameState pTime 1
    clearScreen
    print $ howToPlayText minGuess maxGuess
    gameLoop gameState
