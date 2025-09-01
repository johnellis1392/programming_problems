module AOC2024.Day3Spec (main, spec) where

import AOC2024.Day3
import Data.Functor ((<&>))
import Test.Hspec
import Text.Parsec qualified as P
import Text.RawString.QQ

main :: IO ()
main = hspec spec

testInput :: String
testInput =
  [r|
xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))
|]

testInput2 :: String
testInput2 =
  [r|
xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))
|]

spec :: Spec
spec = do
  describe "parse" $ do
    it "should parse mul expressions" $ do
      P.parse mul "" "mul(1,2)" `shouldBe` Right (Mul 1 2)

    it "should parse a line properly" $ do
      let input :: String = "abcmul(1,2)123mul(3,4)[]mul(5,6),."
      let expected :: [Chunk] =
            [ Mul 1 2,
              Mul 3 4,
              Mul 5 6
            ]
      P.parse parser "" input `shouldBe` Right expected

    it "should pass testInput" $ do
      let expected :: [Chunk] =
            [ Mul 2 4,
              Mul 5 5,
              Mul 11 8,
              Mul 8 5
            ]
      let actual =
            P.parse parser "" testInput
              <&> filter
                ( \case
                    Mul _ _ -> True
                    _ -> False
                )
      actual `shouldBe` Right expected

  describe "part1" $ do
    it "should pass testInput" $ do
      (part1 . parse) testInput `shouldBe` 161

  describe "part2" $ do
    it "should pass testInput2" $ do
      (part2 . parse) testInput2 `shouldBe` 48