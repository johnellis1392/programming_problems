module AOC2020.Day10Spec (main, spec) where

import AOC2020.Day10
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
16
10
15
5
1
11
7
19
6
12
4
|]

testInput2 :: String
testInput2 =
  [r|
28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3
|]

spec :: Spec
spec = do
  describe "parse" $ do
    it "should parse testInput" $ do
      isRight (P.parse parser "Day10Spec" (strip testInput)) `shouldBe` True

  let input = parse testInput
  let input2 = parse testInput2

  describe "part1" $ do
    it "should pass" $ do
      part1 input `shouldBe` (35 :: Int)

    it "should pass case 2" $ do
      part1 input2 `shouldBe` (220 :: Int)

  describe "part2" $ do
    it "should pass" $ do
      part2 input `shouldBe` (8 :: Int)

  it "should pass case 2" $ do
    part2 input2 `shouldBe` (19208 :: Int)