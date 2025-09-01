module AOC2024.Day11Spec (main, spec) where

import AOC2024.Day11
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
0 1 10 99 999
|]

spec :: Spec
spec = do
  describe "parse" $ do
    it "should parse testInput" $ do
      isRight (P.parse parser "Day11Spec" (strip testInput)) `shouldBe` True

  describe "tranform" $ do
    forM_
      [ (0, [1]),
        (1, [2024]),
        (10, [1, 0]),
        (99, [9, 9]),
        (999, [2021976]),
        (253000, [253, 0])
      ]
      $ \(input, expected) -> do
        it ("transform " ++ show input ++ " = " ++ show expected) $ do
          transform input `shouldBe` expected

  describe "run" $ do
    let testCase1 = [125, 17]
    forM_
      [ ([0, 1, 10, 99, 999], 1, [1, 2024, 1, 0, 9, 9, 2021976]),
        (testCase1, 1, [253000, 1, 7]),
        (testCase1, 2, [253, 0, 2024, 14168]),
        (testCase1, 3, [512072, 1, 20, 24, 28676032]),
        (testCase1, 4, [512, 72, 2024, 2, 0, 2, 4, 2867, 6032]),
        (testCase1, 5, [1036288, 7, 2, 20, 24, 4048, 1, 4048, 8096, 28, 67, 60, 32]),
        (testCase1, 6, [2097446912, 14168, 4048, 2, 0, 2, 4, 40, 48, 2024, 40, 48, 80, 96, 2, 8, 6, 7, 6, 0, 3, 2])
      ]
      $ \(input, n, expected) ->
        it ("run (" ++ show n ++ ", " ++ show input ++ ") = " ++ show expected) $ do
          run n input `shouldBe` expected

  describe "part1" $ do
    it "should pass" $ do
      part1 [125, 17] `shouldBe` 55312

-- describe "part2" $ do
--   forM_ testCases $ \(input, _, expected) ->
--     it "should pass" $ do
--     (part2 . parse) input `shouldBe` expected