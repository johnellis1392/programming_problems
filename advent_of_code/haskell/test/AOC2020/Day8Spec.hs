module AOC2020.Day8Spec (main, spec) where

import AOC2020.Day8
import Common.Utils (strip)
import Data.Array (listArray)
import Data.Either (isRight)
import Test.Hspec
import Text.Parsec qualified as P
import Text.RawString.QQ (r)

main :: IO ()
main = hspec spec

testInput :: String
testInput =
  [r|
nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6
|]

spec :: Spec
spec = do
  describe "parse" $ do
    it "should parse a nop" $ do
      P.parse parser "Day8Spec" "nop +0" `shouldBe` Right (listArray (0, 0) [NOP 0])

    it "should parse an acc" $ do
      P.parse parser "Day8Spec" "acc +1" `shouldBe` Right (listArray (0, 0) [ACC 1])

    it "should parse an jmp" $ do
      P.parse parser "Day8Spec" "jmp -1" `shouldBe` Right (listArray (0, 0) [JMP (-1)])

    it "should parse testInput" $ do
      isRight (P.parse parser "Day8Spec" (strip testInput)) `shouldBe` True

  let input = parse testInput

  describe "part1" $ do
    it "should pass" $ do
      part1 input `shouldBe` (5 :: Int)

  describe "part2" $ do
    it "should pass" $ do
      part2 input `shouldBe` (8 :: Int)