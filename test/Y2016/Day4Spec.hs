module Y2016.Day4Spec where

import qualified Data.Map as Map
import SpecHelper
import Y2016.Day4

spec :: Spec
spec = do
  describe "2016 day 4" $ do
    it "parse should work" $ do
      parse "aaaaa-bbb-z-y-x-123[abxyz]" `shouldBe` ("aaaaa-bbb-z-y-x", 123, "abxyz")

    it "countLetters should work" $ do
      let expected = Map.fromList [('a', 5), ('b', 3), ('z', 1), ('y', 1), ('x', 1)]
      countLetters "aaaaabbbzyx" `shouldBe` expected

    it "sortLetters should work" $ do
      let m = Map.fromList [('a', 5), ('b', 3), ('z', 1), ('y', 1), ('x', 1)]
      sortLetters m `shouldBe` [('a', 5), ('b', 3), ('x', 1), ('y', 1), ('z', 1)]

    it "sum of sector IDs" $ do
      indata <- lines <$> readFile "test/Y2016/day4_input.txt"
      sumOfSectors indata `shouldBe` 278221

    it "shiftCipher should work" $ do
      shiftCipher 'q' 343 `shouldBe` 'v'
      shiftCipher 'z' 343 `shouldBe` 'e'
      shiftCipher 'm' 343 `shouldBe` 'r'
      shiftCipher 't' 343 `shouldBe` 'y'

    it "findRealName should work" $ do
      findRealName "qzmt-zixmtkozy-ivhz-343" `shouldBe` ("very encrypted name", 343)

    it "findSectorIdOfNorthPole should work" $ do
      indata <- lines <$> readFile "test/Y2016/day4_input.txt"
      findSectorIdOfNorthPole indata `shouldBe` 267


main :: IO ()
main = hspec spec
