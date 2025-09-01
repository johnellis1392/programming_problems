module AOC2024.Day2Spec (main, spec) where

import AOC2024.Day2
import Test.Hspec
import Text.RawString.QQ

testInput :: String
testInput =
  [r|
7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9
|]

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parse" $ do
    it "should parse input" $ do
      let expected :: Input =
            [ [7, 6, 4, 2, 1],
              [1, 2, 7, 8, 9],
              [9, 7, 6, 2, 1],
              [1, 3, 2, 4, 5],
              [8, 6, 4, 4, 1],
              [1, 3, 6, 7, 9]
            ]
      parse testInput `shouldBe` expected

  describe "part1" $ do
    it "should run correctly" $ do
      (part1 . parse) testInput `shouldBe` 2

  describe "part2" $ do
    it "should run correctly" $ do
      (part2 . parse) testInput `shouldBe` 4