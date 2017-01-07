module Y2016.Day5Spec where

import qualified Data.Map as Map
import SpecHelper
import Y2016.Day5

spec :: Spec
spec = do
  describe "2016 day 5" $ do
    it "password should work" $ do
      password `shouldBe` "c6697b55"

    it "password2 should work" $ do
      password2 getPositionsAndSecrets `shouldBe` "8c35d1ab"


main :: IO ()
main = hspec spec
