module AOC2024.Day12Spec (main, spec) where

import AOC2024.Day12
import Common.Utils (strip)
import Control.Monad (forM_)
import Data.Either (isRight)
import Test.Hspec
import Text.Parsec qualified as P
import Text.RawString.QQ (r)

main :: IO ()
main = hspec spec

testCases :: [(String, Int)]
testCases =
  [ ( [r|
AAAA
BBCD
BBCC
EEEC
|],
      140
    ),
    ( [r|
OOOOO
OXOXO
OOOOO
OXOXO
OOOOO
|],
      772
    ),
    ( [r|
RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE
|],
      1930
    )
  ]

spec :: Spec
spec = do
  describe "parse" $ do
    it "should parse testCases" $ do
      isRight (P.parse parser "Day12Spec" (strip . fst . head $ testCases)) `shouldBe` True

  describe "part1" $ do
    forM_ testCases $ \(input, expected) ->
      it "should pass" $ do
        (part1 . parse) input `shouldBe` expected

  describe "part2" $ do
    it "should pass" $ do
      (part2 . parse . fst . head) testCases `shouldBe` (80 :: Int)