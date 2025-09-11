module AOC2024.Day1Spec (main, spec) where

import AOC2024.Day1
import Test.Hspec
import Text.RawString.QQ

main :: IO ()
main = hspec spec

testInput :: String
testInput =
  [r|
3   4
4   3
2   5
1   3
3   9
3   3
|]

spec :: Spec
spec = do
  describe "parse" $ do
    it "should parse correctly" $ do
      parse testInput `shouldBe` ([3, 4, 2, 1, 3, 3], [4, 3, 5, 3, 9, 3])

  describe "part1" $ do
    it "should do the thing" $ do
      (part1 . parse) testInput `shouldBe` 11

  describe "part2" $ do
    it "should do the thing" $ do
      (part2 . parse) testInput `shouldBe` 31