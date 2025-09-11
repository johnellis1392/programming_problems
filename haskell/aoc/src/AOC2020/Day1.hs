module AOC2020.Day1 (day, parse, part1, part2) where

import Common.Day (Day (Day))
import Common.Day qualified as D
import Common.Utils
import Data.List.Extra (trim)

day :: Day [Int] Int
day =
  Day
    { D.day = "1",
      D.year = "2020",
      D.parse = parse,
      D.part1 = part1,
      D.part2 = part2
    }

parse :: String -> [Int]
parse = fmap parseInt . lines . trim

part1 :: [Int] -> Int
part1 =
  combinations 2
    >.> filter ((== 2020) . sum)
    >.> head
    >.> product

part2 :: [Int] -> Int
part2 =
  combinations 3
    >.> filter ((== 2020) . sum)
    >.> head
    >.> product
