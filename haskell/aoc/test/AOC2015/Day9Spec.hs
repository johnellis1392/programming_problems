module AOC2015.Day9Spec (main, spec) where

import AOC2015.Day9
import Test.Hspec
import Text.RawString.QQ

main :: IO ()
main = hspec spec

testInput :: String
testInput =
  [r|
London to Dublin = 464
London to Belfast = 518
Dublin to Belfast = 141
|]

spec :: Spec
spec = do
  describe "part1" $ do
    it "passes basic test case" $ do
      part1 (parse testInput) `shouldBe` 605

  describe "part2" $ do
    it "passes basic test case" $ do
      part2 (parse testInput) `shouldBe` 982