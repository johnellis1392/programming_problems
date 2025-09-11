module AOC2015.Day5Spec (main, spec) where

import AOC2015.Day5 (nice, nicer)
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "part1" $ do
    it "test case: 'ugknbfddgicrmopn'" $ nice "ugknbfddgicrmopn" `shouldBe` True
    it "test case: 'aaa'" $ nice "aaa" `shouldBe` True
    it "test case: 'jchzalrnumimnmhp'" $ nice "jchzalrnumimnmhp" `shouldBe` False
    it "test case: 'haegwjzuvuyypxyu'" $ nice "haegwjzuvuyypxyu" `shouldBe` False
    it "test case: 'dvszwmarrgswjxmb'" $ nice "dvszwmarrgswjxmb" `shouldBe` False

  describe "part2" $ do
    it "test case: 'qjhvhtzxzqqjkmpb'" $ nicer "qjhvhtzxzqqjkmpb" `shouldBe` True
    it "test case: 'xxyxx'" $ nicer "xxyxx" `shouldBe` True
    it "test case: 'uurcxstgmygtbstg'" $ nicer "uurcxstgmygtbstg" `shouldBe` False
    it "test case: 'ieodomkazucvgmuy'" $ nicer "ieodomkazucvgmuy" `shouldBe` False
