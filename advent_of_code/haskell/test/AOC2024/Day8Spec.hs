module AOC2024.Day8Spec (main, spec) where

import AOC2024.Day8
import Common.Utils (strip)
import Control.Monad (forM_)
import Data.Either (isRight)
import Test.Hspec
import Text.Parsec qualified as P
import Text.RawString.QQ (r)

main :: IO ()
main = hspec spec

testInput :: String
testInput =
  [r|
............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............
|]

spec :: Spec
spec = do
  describe "parse" $ do
    it "should parse testInput" $ do
      isRight (P.parse parser "Day8Spec" (strip testInput)) `shouldBe` True

  describe "antinodePair" $ do
    forM_
      [ ((4, 3), (5, 5), [(6, 7), (3, 1)])
      ]
      $ \(p1, p2, expected) -> do
        it "should pass" $ do
          antinodePair p1 p2 `shouldBe` expected

  let input = parse testInput

  describe "part1" $ do
    it "should pass" $ do
      part1 input `shouldBe` (14 :: Int)

  describe "part2" $ do
    it "should pass" $ do
      part2 input `shouldBe` (34 :: Int)