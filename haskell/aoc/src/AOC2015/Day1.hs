module AOC2015.Day1 (day, parse, part1, part2) where

import Common.Day (Day (Day))
import Common.Day qualified as D

day :: Day [Int] Int
day =
  Day
    { D.day = "1",
      D.year = "2015",
      D.parse = parse,
      D.part1 = part1,
      D.part2 = part2
    }

parse :: String -> [Int]
parse = fmap $ \case
  '(' -> 1
  ')' -> -1
  c -> error $ "Illegal character: " ++ show c

part1 :: [Int] -> Int
part1 = sum

part2 :: [Int] -> Int
part2 = length . takeWhile (>= 0) . scanl (+) 0