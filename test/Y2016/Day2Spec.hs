module Y2016.Day2Spec where

import SpecHelper

spec :: Spec
spec = do
  describe "2016 day 2" $ do
    it "greetings should work" $ do
      greetings `shouldBe` "hello world"


main :: IO ()
main = hspec spec
