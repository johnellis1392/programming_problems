module AOC2015.Day2Spec (main, spec) where

import AOC2015.Day2 (part1, part2)
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "part1" $ do
    it "should pass '2x3x4'" $ part1 [(2, 3, 4)] `shouldBe` 58
    it "should pass '1x1x10'" $ part1 [(1, 1, 10)] `shouldBe` 43

  describe "part2" $ do
    it "should pass '2x3x4'" $ part2 [(2, 3, 4)] `shouldBe` 34
    it "should pass '1x1x10'" $ part2 [(1, 1, 10)] `shouldBe` 14
