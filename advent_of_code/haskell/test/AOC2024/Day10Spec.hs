module AOC2024.Day10Spec (main, spec) where

import AOC2024.Day10
import Common.Utils (strip)
import Control.Monad (forM_)
import Data.Either (isRight)
import Test.Hspec
import Text.Parsec qualified as P
import Text.RawString.QQ (r)

main :: IO ()
main = hspec spec

testCases :: [(String, Int, Int)]
testCases =
  [ ( [r|
0123
1234
8765
9876
|],
      1,
      16
    ),
    ( [r|
89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732
|],
      36,
      81
    )
  ]

spec :: Spec
spec = do
  describe "parse" $ do
    it "should parse testInput" $ do
      isRight (P.parse parser "Day10Spec" (strip ((\(a, _, _) -> a) . head $ testCases))) `shouldBe` True

  describe "part1" $ do
    forM_ testCases $ \(input, expected, _) ->
      it "should pass" $ do
        (part1 . parse) input `shouldBe` expected

  describe "part2" $ do
    forM_ testCases $ \(input, _, expected) ->
      it "should pass" $ do
        (part2 . parse) input `shouldBe` expected