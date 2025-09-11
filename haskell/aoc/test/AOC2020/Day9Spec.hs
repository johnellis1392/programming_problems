module AOC2020.Day9Spec (main, spec) where

import AOC2020.Day9
import Common.Utils (nwise, strip)
import Data.Either (isRight)
import Test.Hspec
import Text.Parsec qualified as P
import Text.RawString.QQ (r)

main :: IO ()
main = hspec spec

testInput :: String
testInput =
  [r|
35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576
|]

spec :: Spec
spec = do
  describe "parse" $ do
    it "should parse testInput" $ do
      isRight (P.parse parser "Day9Spec" (strip testInput)) `shouldBe` True

  describe "nwise" $ do
    it "should chunk list properly" $ do
      let input :: [Int] = [1, 2, 3, 4, 5]
      let expected :: [[Int]] = [[1, 2, 3], [2, 3, 4], [3, 4, 5]]
      nwise 3 input `shouldBe` expected

  let input = (5,) . snd . parse $ testInput

  describe "part1" $ do
    it "should pass" $ do
      part1 input `shouldBe` (127 :: Int)

  describe "part2" $ do
    it "should pass" $ do
      part2 input `shouldBe` (62 :: Int)