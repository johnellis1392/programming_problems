module AOC2015.Day8Spec (main, spec) where

import AOC2015.Day8 (count1, count2, part1, part2)
import Control.Monad (forM_)
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let part1Tests =
        [ ("\"\"", 0),
          ("\"abc\"", 3),
          ("\"aaa\\\"aaa\"", 7),
          ("\"\\x27\"", 1)
        ]

  describe "count1" $ do
    forM_ part1Tests $ \(input, e) -> do
      it ("test case: " ++ show input) $ count1 input `shouldBe` e

  describe "part1" $ do
    it "test case: " $ part1 (fmap fst part1Tests) `shouldBe` 12

  let part2Tests =
        [ ("\"\"", 6),
          ("\"abc\"", 9),
          ("\"aaa\\\"aaa\"", 16),
          ("\"\\x27\"", 11)
        ]

  describe "count2" $ do
    forM_ part2Tests $ \(input, e) -> do
      it ("test case: " ++ show input) $ count2 input `shouldBe` e

  describe "part2" $ do
    it "test case: " $ part2 (fmap fst part2Tests) `shouldBe` 19
