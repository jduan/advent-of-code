module Y2016.Day1Spec where

import SpecHelper

spec :: Spec
spec = do
  describe "2016 day 1" $ do
    it "nextDirection should work" $ do
      nextDirection North L `shouldBe` West


main :: IO ()
main = hspec spec
