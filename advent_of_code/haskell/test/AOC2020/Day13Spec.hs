module AOC2020.Day13Spec (main, spec) where

import AOC2020.Day13
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
939
7,13,x,x,59,x,31,19
|]

spec :: Spec
spec = do
  describe "parse" $ do
    it "should parse testInput" $ do
      isRight (P.parse parser "Day13Spec" (strip testInput)) `shouldBe` True

  describe "part1" $ do
    it "should pass" $ do
      (part1 . parse) testInput `shouldBe` (295 :: Int)

  describe "inv" $ do
    forM_
      [ (3, 11, Just 4),
        (10, 17, Just 12)
      ]
      $ \(a, m, e) -> do
        it ("inv " ++ show a ++ " " ++ show m ++ " == " ++ show e) $ do
          inv a m `shouldBe` e

  describe "crt" $ do
    forM_
      [ ([(2, 3), (3, 4), (1, 5)], 11),
        ([(0, 19), (1, 13), (3, 17)], 3420)
      ]
      $ \(vs, expected) -> do
        it "should calculate crt" $ do
          crt vs `shouldBe` expected

  describe "part2" $ do
    it "should pass" $ do
      let (input, expected) = ("0\n17,x,13,19", 3417)
      (part2 . parse) input `shouldBe` (expected :: Int)

    forM_
      [ ("0\n7,13,x,x,59,x,31,19", 1068781),
        ("0\n17,x,13,19", 3417),
        ("0\n67,7,59,61", 754018),
        ("0\n67,x,7,59,61", 779210),
        ("0\n67,7,x,59,61", 1261476),
        ("0\n1789,37,47,1889", 1202161486)
      ]
      $ \(input, expected) -> do
        it "should pass" $ do
          (part2 . parse) input `shouldBe` (expected :: Int)