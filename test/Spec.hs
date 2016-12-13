import Test.Hspec
import Y2016Day1

main :: IO ()
main = hspec $ do
  describe "2016 day 1" $ do
    it "nextDirection should work" $ do
      nextDirection North L `shouldBe` West
