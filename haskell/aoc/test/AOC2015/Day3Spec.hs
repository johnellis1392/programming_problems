module AOC2015.Day3Spec (main, spec) where

import AOC2015.Day3 (part1, part2)
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "part1" $ do
    it "should pass '>'" $ part1 ">" `shouldBe` 2
    it "should pass '^>v<'" $ part1 "^>v<" `shouldBe` 4
    it "should pass '^v^v^v^v^v'" $ part1 "^v^v^v^v^v" `shouldBe` 2

  describe "part2" $ do
    it "should pass '^v'" $ part2 "^v" `shouldBe` 3
    it "should pass '^>v<'" $ part2 "^>v<" `shouldBe` 3
    it "should pass '^v^v^v^v^v'" $ part2 "^v^v^v^v^v" `shouldBe` 11
