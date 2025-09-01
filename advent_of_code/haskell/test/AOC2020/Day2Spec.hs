module AOC2020.Day2Spec (main, spec) where

import AOC2020.Day2
import Test.Hspec
import Text.RawString.QQ

main :: IO ()
main = hspec spec

testInput :: String
testInput =
  [r|
1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc
|]

spec :: Spec
spec = do
  let input = parse testInput
  describe "part1" $ do
    it "should pass basic test" $ do
      part1 input `shouldBe` 2

  describe "part2" $ do
    it "should pass basic test" $ do
      part2 input `shouldBe` 1
