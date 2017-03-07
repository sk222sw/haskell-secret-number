module Main where

import System.Random
import System.Console.ANSI (clearScreen)

data GameState = GameState { numberToGuess::Integer, numTries::Integer}
                   deriving (Show)

clearAndReturn something = do
  clearScreen
  putStrLn "That was correct!!!"
  return something

gameLoop :: GameState -> IO GameState
gameLoop gs = do
  print $ numberToGuess gs
  putStrLn "Enter a number:"
  s <- getLine
  let num = read s :: Integer
  if num == numberToGuess gs then
                             clearScreen >> return gs
  else gameLoop $ GameState (numberToGuess gs) ((numTries gs) + 1)

main = do
  pTime <- randomRIO(1,4)
  let gameState = GameState pTime 1
  print "Guess a number between 1 and 4"
  gameLoop gameState
