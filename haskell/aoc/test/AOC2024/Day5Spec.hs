module AOC2024.Day5Spec (main, spec) where

import AOC2024.Day5
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
47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47
|]

spec :: Spec
spec = do
  describe "parse" $ do
    it "should parse input" $ do
      isRight (P.parse parser "Day5Spec" (strip testInput)) `shouldBe` True

  let input = parse testInput

  describe "part1" $ do
    it "should pass testInput" $ do
      part1 input `shouldBe` (143 :: Int)

  describe "partitionSort" $ do
    let specMap' = specMap (fst input)
    let testCases :: [([Page], [Page])] =
          [ ([75, 97, 47, 61, 53], [97, 75, 47, 61, 53]),
            ([61, 13, 29], [61, 29, 13]),
            ([97, 13, 75, 29, 47], [97, 75, 47, 29, 13])
          ]
    forM_ testCases $ \(input', expected) -> do
      it ("should sort " ++ show input') $ do
        partitionSort specMap' input' `shouldBe` expected

  describe "part2" $ do
    it "should pass testInput" $ do
      part2 input `shouldBe` (123 :: Int)