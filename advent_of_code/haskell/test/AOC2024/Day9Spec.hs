module AOC2024.Day9Spec (main, spec) where

import AOC2024.Day9
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
2333133121414131402
|]

spec :: Spec
spec = do
  describe "parse" $ do
    it "should parse testInput" $ do
      isRight (P.parse parser "Day9Spec" (strip testInput)) `shouldBe` True

  let input = parse testInput

  describe "expand" $ do
    it "should expand properly" $ do
      (stringify . expand) input `shouldBe` "00...111...2...333.44.5555.6666.777.888899"

  describe "defrag" $ do
    it "should pass simple test" $ do
      (stringify . defrag . expand . parse) "12345" `shouldBe` "022111222"

    it "should pass more complex test" $ do
      (stringify . defrag . expand) input `shouldBe` "0099811188827773336446555566"

  describe "checksum" $ do
    it "should pass basic test" $ do
      let i = [Full 0 1, Full 2 2, Full 1 3, Full 2 3]
      checksum i `shouldBe` 60

  describe "part1" $ do
    it "should pass" $ do
      part1 input `shouldBe` (1928 :: Int)

  describe "part2" $ do
    it "should pass" $ do
      part2 input `shouldBe` (2858 :: Int)