module AOC2015.Day1Spec (main, spec) where

import AOC2015.Day1
import Control.Monad (forM_)
import Test.Hspec

main :: IO ()
main = hspec spec

type TestCase = (String, Int)

part1Tests :: [TestCase]
part1Tests =
  [ ("(())", 0),
    ("()()", 0),
    ("(((", 3),
    ("(()(()(", 3),
    ("))(((((", 3),
    ("())", -1),
    ("))(", -1),
    (")))", -3),
    (")())())", -3)
  ]

part2Tests :: [TestCase]
part2Tests =
  [ (")", 1),
    ("()())", 5)
  ]

spec :: Spec
spec = do
  describe "part1" $ do
    forM_ part1Tests $ \(input, expected) ->
      it ("test case: '" ++ input ++ "' == " ++ show expected) $ do
        part1 (parse input) `shouldBe` expected

  describe "part2" $ do
    forM_ part2Tests $ \(input, expected) ->
      it ("test case: '" ++ input ++ "' == " ++ show expected) $ do
        part2 (parse input) `shouldBe` expected
