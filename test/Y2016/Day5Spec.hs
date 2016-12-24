module Y2016.Day5Spec where

import qualified Data.Map as Map
import SpecHelper
import Y2016.Day5

spec :: Spec
spec = do
  describe "2016 day 5" $ do
    it "password should work" $ do
      password `shouldBe` "c6697b55"


main :: IO ()
main = hspec spec
