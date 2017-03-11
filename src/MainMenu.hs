module MainMenu where

import System.Console.ANSI
import SecretNumberGame (initGame)

getInstructions :: String
getInstructions = 
    "Guess a number from 1 to 100. You will know if your guess was too high or too low. You can't make a guess that is lower or higher than a previous guess, i.e. if the answer is 44, and you guess 20, you can't make a guess lower than 21 again. Cause why would you? Have fun!"

showMenu :: IO ()
showMenu = do
    putStrLn "Secret number!"
    putStrLn "1. Play"
    putStrLn "2. Instructions"
    putStrLn "3. Exit"
    option <- getChar
    case option of
        '1' -> initGame
        '2' -> do
            clearScreen
            putStrLn "--- How to play ---"
            putStrLn getInstructions
            putStrLn ""
            putStrLn "Press any key to go back."
            getChar
            clearScreen
            putStrLn ""
            showMenu
        '3' -> clearScreen
        _   -> mainMenu

mainMenu = clearScreen >> showMenu
