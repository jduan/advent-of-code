module Y2016.Day1Spec where

import SpecHelper

input =
  "L3, R2, L5, R1, L1, L2, L2, R1, R5, R1, L1, L2, R2, R4, L4, L3, L3, R5, L1, R3, L5, L2, R4, L5, R4, R2, L2, L1, R1, L3, L3, R2, R1, L4, L1, L1, R4, R5, R1, L2, L1, R188, R4, L3, R54, L4, R4, R74, R2, L4, R185, R1, R3, R5, L2, L3, R1, L1, L3, R3, R2, L3, L4, R1, L3, L5, L2, R2, L1, R2, R1, L4, R5, R4, L5, L5, L4, R5, R4, L5, L3, R4, R1, L5, L4, L3, R5, L5, L2, L4, R4, R4, R2, L1, L3, L2, R5, R4, L5, R1, R2, R5, L2, R4, R5, L2, L3, R3, L4, R3, L2, R1, R4, L5, R1, L5, L3, R4, L2, L2, L5, L5, R5, R2, L5, R1, L3, L2, L2, R3, L3, L4, R2, R3, L1, R2, L5, L3, R4, L4, R4, R3, L3, R1, L3, R5, L5, R1, R5, R3, L1"

spec :: Spec
spec = do
  describe "2016 day 1" $ do
    it "nextDirection should work" $ do
      nextDirection North L `shouldBe` West
    it "blocksAway should work" $ do
      blocksAway (Coordinate 3 4) `shouldBe` 7
      blocksAway (Coordinate (-3) 4) `shouldBe` 7
      blocksAway (Coordinate (-3) (-4)) `shouldBe` 7
    it "getMoves should work" $ do
      getMoves(["L3", "R2", "L5", "R1", "L1", "L2"]) `shouldBe` [
        Move L 3, Move R 2, Move L 5, Move R 1, Move L 1, Move L 2]
    it "getMoves should work" $ do
      calculateBlocks input `shouldBe` Coordinate (-131) 78
      calculateBlocks "L3, R2, L5, R1, L1, L2, L2, R1" `shouldBe` Coordinate (-7) 0

main :: IO ()
main = hspec spec
