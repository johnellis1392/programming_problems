module AOC2024.Day13Spec (main, spec) where

import AOC2024.Day13
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
Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279
|]

spec :: Spec
spec = do
  describe "parse" $ do
    it "should parse testCases" $ do
      isRight (P.parse parser "Day13Spec" (strip testInput)) `shouldBe` True

  let testCases = parse testInput

  describe "matrix math" $ do
    let expected = [Just (80, 40), Nothing, Just (38, 86), Nothing]
    forM_ (zip testCases expected) $ \((a, b, p), e) ->
      it "should pass" $ do
        let m = toMat a b
        sle m p `shouldBe` e

  describe "cost" $ do
    let expected = [Just 280, Nothing, Just 200, Nothing]
    forM_ (zip testCases expected) $ \((a, b, p), e) ->
      it "should pass" $ do
        cost (a, b, p) `shouldBe` e

  describe "part1" $ do
    it "should pass" $ do
      (part1 . parse) testInput `shouldBe` 480

-- describe "part2" $ do
--   it "should pass" $ do
--     (part2 . parse . fst . head) testCases `shouldBe` (80 :: Int)