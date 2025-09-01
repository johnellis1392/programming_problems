module AOC2020.Day3Spec (main, spec) where

import AOC2020.Day3
import Test.Hspec
import Text.RawString.QQ

main :: IO ()
main = hspec spec

testInput :: String
testInput =
  [r|
..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#
|]

spec :: Spec
spec = do
  let input = parse testInput
  describe "part1" $ do
    it "should pass basic test" $ do
      part1 input `shouldBe` 7

  describe "part2" $ do
    it "should pass basic test" $ do
      part2 input `shouldBe` 336
