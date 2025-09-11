module AOC2020.Day7Spec (main, spec) where

import AOC2020.Day7
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
light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.
|]

testInput2 :: String
testInput2 =
  [r|
shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags.
|]

spec :: Spec
spec = do
  describe "parse" $ do
    it "should parse bag names" $ do
      P.parse bagName "" "light red" `shouldBe` Right "light red"

    forM_
      ( [ ("5 dark olive bags", ("dark olive", 5)),
          ("1 dotted black bag", ("dotted black", 1))
        ] ::
          [(String, (String, Int))]
      )
      $ \(input, expected) -> do
        it ("should parse bag entry: \"" ++ input ++ "\"") $ do
          P.parse bagEntry "" input `shouldBe` Right expected

    forM_
      ( [ ("5 faded blue bags, 6 dotted black bags.", [("faded blue", 5), ("dotted black", 6)]),
          ("1 dotted black bag.", [("dotted black", 1)]),
          ("no other bags.", [])
        ] ::
          [(String, [(String, Int)])]
      )
      $ \(input, expected) -> do
        it "should parse a bag list" $ do
          P.parse bagList "" input `shouldBe` Right expected

    forM_
      ( [ ("vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.", ("vibrant plum", [("faded blue", 5), ("dotted black", 6)])),
          ("faded blue bags contain no other bags.", ("faded blue", [])),
          ("bright white bags contain 1 shiny gold bag.", ("bright white", [("shiny gold", 1)]))
        ] ::
          [(String, Bag)]
      )
      $ \(input, expected) -> do
        it "should parse a bag" $ do
          P.parse bag "" input `shouldBe` Right expected

    it "should parse testInput" $ do
      isRight (P.parse parser "Day7Spec" (strip testInput)) `shouldBe` True

  let input = parse testInput

  describe "part1" $ do
    it "should pass testInput" $ do
      part1 input `shouldBe` (4 :: Int)

  describe "part2" $ do
    it "should pass testInput" $ do
      part2 input `shouldBe` (32 :: Int)

    it "should pass another test" $ do
      let input2 = parse testInput2
      part2 input2 `shouldBe` (126 :: Int)