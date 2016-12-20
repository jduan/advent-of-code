module Y2016.Day3Spec where

import SpecHelper
import Y2016.Day3

spec :: Spec
spec = do
  describe "2016 day 3" $ do
    it "countValidTriangles should work" $ do
      indata <- lines <$> readFile "test/Y2016/day3_input.txt"
      let tris = map words indata
      countValidTriangles tris `shouldBe` 862

    it "countValidTriangles2 should work" $ do
      indata <- lines <$> readFile "test/Y2016/day3_input.txt"
      let tris = map words indata
      countValidTriangles2 tris `shouldBe` 1577

    it "countValidTriangles2' should work" $ do
      indata <- lines <$> readFile "test/Y2016/day3_input.txt"
      let tris = map words indata
      countValidTriangles2' tris `shouldBe` 1577

main :: IO ()
main = hspec spec
