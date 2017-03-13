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
               correctGuessText (GameState 2 2 (1, 100) []) `shouldBe`
               "2 is correct! It took you 2 tries."
           it "should only pluralize if not provded a 1" $
               correctGuessText (GameState 2 2 (1, 100) []) `shouldBe`
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
            guessIsInRange (23, 48) 23  `shouldBe` False
            guessIsInRange (23, 48) 48  `shouldBe` False
    describe "calculateNewRange" $ do
        it "should make the lower range the guess if guess is too low" $ do
            let oldRange = (34,93)
            let answer = 55
            let guess = 40
            calculateNewRange oldRange answer guess `shouldBe` (40,93)
        it "should make the lower range the guess if guess is too low" $
            calculateNewRange (34,93) 55 70 `shouldBe` (34,70)
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