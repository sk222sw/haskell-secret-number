{-# Language OverloadedStrings #-}

module Main where

import System.Random
import System.Console.ANSI (clearScreen)
import Text.Read (readMaybe)

data GameState = GameState { numberToGuess::Integer, numTries::Integer}
                   deriving (Show)

clearAndReturn something = do
  clearScreen
  putStrLn "That was correct!"
  return something

safeRead :: IO Integer
safeRead = do
  s <- getLine
  case readMaybe s :: Maybe Integer of
    Nothing -> do
      putStrLn "Only numbers"
      safeRead
    Just s -> return s

gameLoop :: GameState -> IO GameState
gameLoop gs = do
  print $ numberToGuess gs
  putStrLn "Enter a number:"
  s <- safeRead
  let num = s :: Integer
  if num == numberToGuess gs then
                             clearScreen >> return gs
  else gameLoop $ GameState (numberToGuess gs) (numTries gs + 1)

main = do
  pTime <- randomRIO(1,4)
  let gameState = GameState pTime 1
  print "Guess a number between 1 and 4"
  gameLoop gameState
