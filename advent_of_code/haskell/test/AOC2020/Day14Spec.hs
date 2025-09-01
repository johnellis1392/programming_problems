module AOC2020.Day14Spec (main, spec) where

import AOC2020.Day14
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
mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0
|]

spec :: Spec
spec = do
  describe "parse" $ do
    it "should parse testInput" $ do
      isRight (P.parse parser "Day14Spec" (strip testInput)) `shouldBe` True

  describe "masks" $ do
    it "zeroMask" $ do
      let expected = 0b1111__1111_1111_1111_1111__1111_1111_1110_1101
      let input = [4, 1]
      zeroMask input `shouldBe` expected

    it "oneMask" $ do
      let expected = 0b1_0010
      let input = [4, 1]
      oneMask input `shouldBe` expected

    it "stringToMask" $ do
      let input = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
      let expected = (zeroMask [1], oneMask [6])
      stringToMask input `shouldBe` expected

  describe "part1" $ do
    it "should pass" $ do
      (part1 . parse) testInput `shouldBe` (165 :: Int)

-- describe "part2" $ do
--   it "should pass" $ do
--     let (input, expected) = ("0\n17,x,13,19", 3417)
--     (part2 . parse) input `shouldBe` (expected :: Int)