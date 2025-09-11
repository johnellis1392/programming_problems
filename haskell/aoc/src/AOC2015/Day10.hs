module AOC2015.Day10 (lookAndSay, part1, part2, parse, day) where

import Common.Day (Day (Day))
import Common.Day qualified as D
import Common.Utils (strip)
import Data.List (group, singleton)
import Data.Tuple.Extra ((&&&))

day :: Day String String
day =
  Day
    { D.day = "10",
      D.year = "2015",
      D.parse = parse,
      D.part1 = part1 0,
      D.part2 = part2 0
    }

lookAndSay :: String -> String
lookAndSay = foldl1 (++) . fmap (uncurry (++) . ((show . length) &&& (singleton . head))) . group

parse :: String -> String
parse = strip

part1 :: Int -> String -> String
part1 0 s = s
part1 i s =
  let v = lookAndSay s
   in part1 (i - 1) v

part2 :: Int -> String -> String
part2 _ _ = ""