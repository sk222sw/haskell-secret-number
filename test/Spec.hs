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
               correctGuessText (GameState 2 2) `shouldBe`
               "2 is correct! It took you 2 tries."
           it "should only pluralize if not provded a 1" $
               correctGuessText (GameState 2 2) `shouldBe`
               "2 is correct! It took you 2 tries."
    describe "validGuess" $
        it "checks if a string is an integer" $ do
          validGuess "hej" `shouldBe` False  
          validGuess "1" `shouldBe` True  
          validGuess "1.23" `shouldBe` False  