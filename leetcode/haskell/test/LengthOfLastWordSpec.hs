{-# LANGUAGE OverloadedStrings #-}

module LengthOfLastWordSpec (spec) where

import Test.Hspec
import LengthOfLastWord (lengthOfLastWord)
import qualified Data.Text as T

spec :: Spec
spec = do
  describe "plusOne" $ do
    it "test1" $ lengthOfLastWord "Hello World" `shouldBe` 5
    it "test2" $ lengthOfLastWord "   fly me   to   the moon  " `shouldBe` 4
    it "test3" $ lengthOfLastWord "luffy is still joyboy" `shouldBe` 6
