module AOC2024.Day4Spec (main, spec) where

import AOC2024.Day4
import Data.Either (isRight)
import Test.Hspec
import Text.Parsec qualified as P
import Text.RawString.QQ (r)

main :: IO ()
main = hspec spec

testInput :: String
testInput =
  [r|
MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX
|]

spec :: Spec
spec = do
  describe "parse" $ do
    it "should parse input" $ do
      isRight (P.parse parser "Day4Spec" testInput) `shouldBe` True

  describe "part1" $ do
    it "should pass testInput" $ do
      let expected = 18 :: Int
      (part1 . parse) testInput `shouldBe` expected

  describe "part2" $ do
    it "should pass testInput" $ do
      let expected = 9 :: Int
      (part2 . parse) testInput `shouldBe` expected