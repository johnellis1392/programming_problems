module AOC2020.Day5Spec (main, spec) where

import AOC2020.Day5
import Control.Monad (forM_)
import Test.Hspec
import Text.RawString.QQ (r)

main :: IO ()
main = hspec spec

testInput :: String
testInput =
  [r|
FBFBBFFRLR
BFFFBBFRRR
FFFBBBFRRR
BBFFBBFRLL
|]

spec :: Spec
spec = do
  describe "parse" $ do
    it "should parse basic input" $ do
      let input = "FBFBBFFRLR"
      parse input `shouldBe` [[F, B, F, B, B, F, F, R, L, R]]

  describe "seatAssignment" $ do
    let testCases :: [(String, Seat)] =
          [ ("FBFBBFFRLR", Seat 44 5),
            ("BFFFBBFRRR", Seat 70 7),
            ("FFFBBBFRRR", Seat 14 7),
            ("BBFFBBFRLL", Seat 102 4)
          ]
    forM_ testCases $ \(input, expected) ->
      it ("seatAssignment(\"" ++ input ++ "\") = " ++ show expected) $ do
        (seatAssignment . head . parse) input `shouldBe` expected

  describe "part1" $ do
    it "should pass" $ do
      let expected = 820
      (part1 . parse) testInput `shouldBe` expected