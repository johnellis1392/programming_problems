module PlusOneSpec (spec) where

import Test.Hspec
import PlusOne (plusOne)
-- import Test.QuickCheck

spec :: Spec
spec = do
  describe "plusOne" $ do
    it "test1" $ plusOne [1,2,3] `shouldBe` [1,2,4]
    it "test2" $ plusOne [4,3,2,1] `shouldBe` [4,3,2,2]
    it "test3" $ plusOne [9] `shouldBe` [1,0]
