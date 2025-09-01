module AOC2015.Day2 (part1, part2, parse, day) where

import Common.Day (Day (Day))
import Common.Day qualified as D
import Common.Utils (fork)
import Data.List (sort)
import Data.Text (Text, pack, splitOn, strip, unpack)

type Gift = (Int, Int, Int)

day :: Day [Gift] Int
day =
  Day
    { D.day = "2",
      D.year = "2015",
      D.parse = parse,
      D.part1 = part1,
      D.part2 = part2
    }

parseInt :: Text -> Int
parseInt s = (read . unpack) s :: Int

parse :: String -> [Gift]
parse = fmap parseGift . splitOn "\n" . strip . pack
  where
    parseGift :: Text -> Gift
    parseGift s =
      case fmap parseInt . splitOn "x" $ s of
        [l, w, h] -> (l, w, h)
        _ -> error $ "illegal pattern: " ++ show s

part1 :: [Gift] -> Int
part1 = sum . fmap (fork paper (+) slack)
  where
    sides :: Gift -> [Int]
    sides (l, w, h) = [l * w, l * h, w * h]

    paper :: Gift -> Int
    paper = (2 *) . sum . sides

    slack :: Gift -> Int
    slack = minimum . sides

part2 :: [Gift] -> Int
part2 =
  sum
    . fmap
      ( fork
          ((* 2) . sum . take 2 . sort . untuple)
          (+)
          (product . untuple)
      )
  where
    untuple :: Gift -> [Int]
    untuple (a, b, c) = [a, b, c]