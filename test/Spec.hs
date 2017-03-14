import Test.Hspec
import SecretNumberGame

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "tooLowOrTooHighText" $
        it "should return too low if guess is lower than answer" $
        do tooLowOrTooHighText 2 1 `shouldBe` "Too low"
           tooLowOrTooHighText 2 3 `shouldBe` "Too high"
    describe "correctGuessText" $
        do it "should show a text with the correct answer and number of tries" $
               correctGuessText (GameState 2 2 (1, 100) [] None) `shouldBe`
               "2 is correct! It took you 2 tries."
           it "should only pluralize if not provded a 1" $
               correctGuessText (GameState 2 2 (1, 100) [] None) `shouldBe`
               "2 is correct! It took you 2 tries."
    describe "validGuess" $
        it "checks if a string is an integer" $ do
            validGuess "hej" `shouldBe` False  
            validGuess "1" `shouldBe` True  
            validGuess "1.23" `shouldBe` False 
    describe "guessIsInRange" $
        it "checks if a value is within previous guesses" $ do
            guessIsInRange (23, 48) 28  `shouldBe` True
            guessIsInRange (23, 48) 1   `shouldBe` False
            guessIsInRange (23, 48) 99  `shouldBe` False
            guessIsInRange (23, 48) 23  `shouldBe` True
            guessIsInRange (23, 48) 48  `shouldBe` True
    describe "calculateNewRange" $ do
        it "should make the lower range the guess+1 if guess is too low" $ do
            let oldRange = (34,93)
            let answer = 55
            let guess = 40
            calculateNewRange oldRange answer guess `shouldBe` (41,93)
        it "should make the higher range the guess-1 if guess is too high" $
            calculateNewRange (34,93)  55     90    `shouldBe` (34,89)
    describe "outOfRangeText" $
        it "should show a text with the guess range in it" $
            outOfRangeText (24, 87) `shouldBe` "Your guess should be between 24 and 87"
    describe "guessRangeText" $
        it "should display the correct range" $ do
            let range = (12, 94)
            guessRangeText range `shouldBe` "Valid guesses: 12 to 94"
    describe "previousGuessesText" $
        it "should display previous guesses" $ do
            let guesses = [50, 25, 13]
            previousGuessesText guesses `shouldBe` "Previous guesses: 50 25 13"
    describe "guessStatus" $
        it "should return the correct guess status" $ do
            let guess = 40
            let answer = 50
            guessStatus guess answer `shouldBe` Low
            guessStatus 30    100    `shouldBe` Low
            guessStatus 50    50     `shouldBe` Correct
    describe "updateGuessRangeInState" $
        it "should return the game state with an updated range" $ do
            let oldState = GameState 23 2 (23, 90) [22, 91] Low
            let expected = GameState 23 2 (30, 90) [22, 91] Low
            let newRange = (30, 90)
            updateGuessRangeInState oldState newRange `shouldBe` expected

