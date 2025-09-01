module AOC2020.Day2 (day, parse, part1, part2) where

import Common.Day (Day (Day))
import Common.Day qualified as D
import Common.Utils (count, parseInt, (>.>))
import Data.List.Extra (trim)

day :: Day [Input] Int
day =
  Day
    { D.day = "2",
      D.year = "2020",
      D.parse = parse,
      D.part1 = part1,
      D.part2 = part2
    }

type Input = ((Int, Int), Char, String)

parse :: String -> [Input]
parse = trim >.> lines >.> fmap (parseLine . words)
  where
    parseLine :: [String] -> Input
    parseLine [range, [char, ':'], password] =
      let (start, end) = break (== '-') range
       in ((parseInt start, parseInt $ drop 1 end), char, password)
    parseLine other = error $ "Illegal input: " ++ show other

part1 :: [Input] -> Int
part1 = count valid
  where
    valid :: Input -> Bool
    valid ((start, end), c, password) =
      let n = count (== c) password
       in start <= n && n <= end

-- 524 too high
part2 :: [Input] -> Int
part2 = count valid
  where
    valid :: Input -> Bool
    valid ((start, end), c, password) =
      (c == password !! (start - 1))
        `xor` (c == password !! (end - 1))

    xor :: Bool -> Bool -> Bool
    xor True False = True
    xor False True = True
    xor _ _ = False
