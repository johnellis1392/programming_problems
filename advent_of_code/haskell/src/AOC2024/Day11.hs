module AOC2024.Day11 (parser, parse, part1, part2, day, run, transform, update) where

import Common.Day qualified as D
import Common.Utils (parseWith, strip)
import Control.Arrow (first)
import Data.Functor ((<&>))
import Data.HashMap.Lazy qualified as M
import Text.Parsec hiding (State, parse)
import Text.Parsec.String

day :: D.Day Input Int
day =
  D.Day
    { D.day = "11",
      D.year = "2024",
      D.parse = parse,
      D.part1 = part1,
      D.part2 = part2
    }

type Input = [Int]

parser :: Parser Input
parser = (many1 digit `sepBy` space <* eof) <&> fmap (read @Int)

parse :: String -> Input
parse = parseWith parser "Day11" . strip

transform :: Int -> [Int]
transform v
  | v == 0 = [1]
  | log10 v `mod` 2 == 1 =
      let n = (log10 v + 1) `div` 2
       in [v `div` (10 ^ n), v `mod` (10 ^ n)]
  | otherwise = [v * 2024]
  where
    log10 :: Int -> Int
    log10 = floor . logBase 10 . (fromIntegral :: Int -> Double)

update :: [Int] -> [Int]
update vs = vs >>= transform

run :: Int -> [Int] -> [Int]
run 0 vs = vs
run n vs = run (n - 1) (update vs)

part1 :: Input -> Int
part1 = length . run 25

type Cache = M.HashMap (Int, Int) Int

run' :: Cache -> Int -> Int -> (Int, Cache)
run' cache 0 _ = (1, cache)
run' cache depth n
  | (n, depth) `M.member` cache = (,cache) $ cache M.! (n, depth)
  | otherwise =
      let vs = transform n
          (res, cache'') = foldl (\(sum', cache') i -> first (+ sum') . run' cache' (depth - 1) $ i) (0, cache) vs
          cache''' = M.insert (n, depth) res cache''
       in (res, cache''')

part2 :: Input -> Int
part2 vs =
  let cache :: Cache = M.empty
      depth = 75
      (res, _) = foldl (\(sum', cache') i -> first (+ sum') . run' cache' depth $ i) (0, cache) vs
   in res