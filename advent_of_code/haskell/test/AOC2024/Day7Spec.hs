module AOC2024.Day7Spec (main, spec) where

import AOC2024.Day7
import Common.Utils (strip)
import Data.Either (isRight)
import Test.Hspec
import Text.Parsec qualified as P
import Text.RawString.QQ (r)

main :: IO ()
main = hspec spec

testInput :: String
testInput =
  [r|
190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20
|]

spec :: Spec
spec = do
  describe "parse" $ do
    it "should parse a single number" $ do
      P.parse parser "Day7" "123: 456" `shouldBe` Right [(123, [456])]

    it "should parse multiple numbers" $ do
      P.parse parser "Day7" "123: 456 789" `shouldBe` Right [(123, [456, 789])]

    it "should parse testInput" $ do
      isRight (P.parse parser "Day7" (strip testInput)) `shouldBe` True

  let input = parse testInput

  describe "part1" $ do
    it "should pass" $ do
      part1 input `shouldBe` (3749 :: Int)

  describe "part2" $ do
    it "should pass" $ do
      part2 input `shouldBe` (11387 :: Int)