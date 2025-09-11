module AOC2020.Day6Spec (main, spec) where

import AOC2020.Day6
import Test.Hspec
import Text.RawString.QQ (r)

testInput :: String
testInput =
  [r|
abc

a
b
c

ab
ac

a
a
a
a

b
|]

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parse" $ do
    it "should parse a line" $ do
      parse "asdf" `shouldBe` [["asdf"]]

    it "should parse a group" $ do
      parse "a\nb\nc" `shouldBe` [["a", "b", "c"]]

    it "should parse multiple groups" $ do
      parse "a\nb\nc\n\nd\ne\nf\n\ng\nh\ni" `shouldBe` [["a", "b", "c"], ["d", "e", "f"], ["g", "h", "i"]]

  describe "part1" $ do
    it "should count answers correctly" $ do
      let expected :: Int = 11
      (part1 . parse) testInput `shouldBe` expected