module AOC2024.Day14Spec (main, spec) where

import AOC2024.Day14
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
p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3
|]

spec :: Spec
spec = do
  describe "parse" $ do
    it "should parse testCases" $ do
      isRight (P.parse parser "Day14Spec" (strip testInput)) `shouldBe` True

  let input = parse testInput

  describe "part1" $ do
    it "should pass" $ do
      part1' input (11, 7) `shouldBe` 12

-- describe "part2" $ do
--   it "should pass" $ do
--     (part2 . parse . fst . head) testCases `shouldBe` (80 :: Int)