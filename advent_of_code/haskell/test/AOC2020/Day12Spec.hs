module AOC2020.Day12Spec (main, spec) where

import AOC2020.Day12
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
F10
N3
F7
R90
F11
|]

spec :: Spec
spec = do
  describe "parse" $ do
    it "should parse testInput" $ do
      isRight (P.parse parser "Day12Spec" (strip testInput)) `shouldBe` True

  let input = parse testInput

  describe "part1" $ do
    it "should pass" $ do
      part1 input `shouldBe` (25 :: Int)

  describe "part2" $ do
    it "should pass" $ do
      part2 input `shouldBe` (286 :: Int)