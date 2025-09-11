module AOC2015.Day10Spec (main, spec) where

import AOC2015.Day10
import Control.Monad (forM_)
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "lookAndSay" $ do
    let testCases =
          [ ("1", "11"),
            ("11", "21"),
            ("21", "1211"),
            ("1211", "111221"),
            ("111221", "312211")
          ] ::
            [(String, String)]
    forM_ testCases $ \(input, e) ->
      it (show input ++ " == " ++ show e) $ lookAndSay input `shouldBe` e

  describe "part1" $ do
    it "should pass test case: " $ part1 5 "1" `shouldBe` "312211"
