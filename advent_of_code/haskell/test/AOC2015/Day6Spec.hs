module AOC2015.Day6Spec (main, spec) where

import AOC2015.Day6 (Command (..), part1, part2)
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "part1" $ do
    it "test case: turn on 0,0 through 999,999" $ do
      res <- part1 [TurnOn (0, 0) (999, 999)]
      res `shouldBe` 1000000
    it "test case: toggle 0,0 through 999,0" $ do
      res <- part1 [Toggle (0, 0) (999, 0)]
      res `shouldBe` 1000

  describe "part2" $ do
    it "test case: turn on 0,0 through 0,0" $ do
      res <- part2 [TurnOn (0, 0) (0, 0)]
      res `shouldBe` 1
    it "test case: toggle 0,0 through 999,999" $ do
      res <- part2 [Toggle (0, 0) (999, 999)]
      res `shouldBe` 2000000
