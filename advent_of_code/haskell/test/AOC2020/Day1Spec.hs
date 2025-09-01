module AOC2020.Day1Spec (main, spec) where

import AOC2020.Day1
import Test.Hspec
import Text.RawString.QQ

main :: IO ()
main = hspec spec

testInput :: String
testInput =
  [r|
1721
979
366
299
675
1456
|]

spec :: Spec
spec = do
  let input = parse testInput
  describe "part1" $ do
    it "should pass basic test" $ do
      part1 input `shouldBe` 514579

  describe "part2" $ do
    it "should pass basic test" $ do
      part2 input `shouldBe` 241861950