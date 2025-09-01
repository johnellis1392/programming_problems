module AOC2015.Day9 (part1, part2, parse, day) where

import Common.Day (Day (Day))
import Common.Day qualified as D
import Common.Utils (strip)
import Data.Functor ((<&>))
import Data.List (permutations)
import Data.Map ((!))
import Data.Map qualified as M
import Data.Set qualified as S

data Direction = Direction String String Int

day :: Day [Direction] Int
day =
  Day
    { D.day = "9",
      D.year = "2015",
      D.parse = parse,
      D.part1 = part1,
      D.part2 = part2
    }

parseInt :: String -> Int
parseInt = read

parse :: String -> [Direction]
parse input = (lines . strip) input <&> parse' . words
  where
    parse' [source, "to", dest, "=", dist] = Direction source dest (parseInt dist)
    parse' v = error $ "Illegal value: " ++ show v

pairs :: [a] -> [(a, a)]
pairs (a : b : rest) = (a, b) : pairs (b : rest)
pairs _ = []

part1 :: [Direction] -> Int
part1 directions =
  let places = S.elems $ foldl (\set (Direction s d _) -> S.insert d (S.insert s set)) S.empty directions
   in minimum . fmap (sum . fmap (distances !) . pairs) . permutations $ places
  where
    distances :: M.Map (String, String) Int
    distances = M.fromList $ foldl1 (++) $ directions <&> \(Direction s d d') -> [((d, s), d'), ((s, d), d')]

part2 :: [Direction] -> Int
part2 directions =
  let places = S.elems $ foldl (\set (Direction s d _) -> S.insert d (S.insert s set)) S.empty directions
   in maximum . fmap (sum . fmap (distances !) . pairs) . permutations $ places
  where
    distances :: M.Map (String, String) Int
    distances = M.fromList $ foldl1 (++) $ directions <&> \(Direction s d d') -> [((s, d), d'), ((d, s), d')]