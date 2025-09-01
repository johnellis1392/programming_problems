module AOC2015.Day3 (part1, part2, parse, day) where

import Common.Day (Day (Day))
import Common.Day qualified as D
import Common.Utils (strip, zipWithIndex)
import Data.List qualified as L
import Data.Set qualified as S
import Data.Tuple.Extra (both)

day :: Day String Int
day =
  Day
    { D.day = "3",
      D.year = "2015",
      D.parse = parse,
      D.part1 = part1,
      D.part2 = part2
    }

translate :: Char -> (Int, Int)
translate '^' = (0, 1)
translate 'v' = (0, -1)
translate '>' = (1, 0)
translate '<' = (-1, 0)
translate c = error $ "Illegal character: " ++ show c

(<+>) :: (Int, Int) -> (Int, Int) -> (Int, Int)
(<+>) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

parse :: String -> String
parse = strip

part1 :: String -> Int
part1 = length . S.fromList . scanl (<+>) (0, 0) . fmap translate

part2 :: String -> Int
part2 =
  length
    . combine S.union
    . both S.fromList
    . both (scanl (<+>) (0, 0))
    . both (fmap (translate . snd))
    . L.partition (even . fst)
    . zipWithIndex
  where
    combine :: (a -> b -> c) -> (a, b) -> c
    combine f (a, b) = f a b