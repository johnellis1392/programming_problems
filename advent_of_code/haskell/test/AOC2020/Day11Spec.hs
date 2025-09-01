module AOC2020.Day11Spec (main, spec) where

import AOC2020.Day11
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
L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL
|]

spec :: Spec
spec = do
  describe "parse" $ do
    it "should parse testInput" $ do
      isRight (P.parse parser "Day11Spec" (strip testInput)) `shouldBe` True

  let input = parse testInput

  describe "part1" $ do
    it "should pass" $ do
      part1 input `shouldBe` (37 :: Int)

  describe "part2" $ do
    it "should pass" $ do
      part2 input `shouldBe` (26 :: Int)