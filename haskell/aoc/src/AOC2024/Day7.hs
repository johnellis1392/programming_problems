{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module AOC2024.Day7 (parser, parse, part1, part2, day, line) where

import Common.Day qualified as D
import Common.Utils (parseWith, strip)
import Text.Parsec hiding (count, parse)
import Text.Parsec.String

day :: D.Day Input Int
day =
  D.Day
    { D.day = "7",
      D.year = "2024",
      D.parse = parse,
      D.part1 = part1,
      D.part2 = part2
    }

type Input = [(Int, [Int])]

line :: Parser (Int, [Int])
line = do
  n <- many1 digit
  string ": "
  ns <- many1 digit `sepBy1` char ' '
  return (read n, map read ns)

parser :: Parser Input
parser = line `sepBy1` newline <* eof

parse :: String -> Input
parse = parseWith parser "Day7" . strip

part1 :: Input -> Int
part1 = sum . fmap fst . filter (uncurry f)
  where
    f :: Int -> [Int] -> Bool
    f _ [] = False
    f n [x] = n == x
    f n (x0 : x1 : xs) = f n (x0 + x1 : xs) || f n (x0 * x1 : xs)

part2 :: Input -> Int
part2 = sum . fmap fst . filter (uncurry f)
  where
    f :: Int -> [Int] -> Bool
    f _ [] = False
    f n [x] = n == x
    f n (x0 : x1 : xs) =
      f n (x0 + x1 : xs)
        || f n (x0 * x1 : xs)
        || f n (x0 * 10 ^ (1 + log10 x1) + x1 : xs)

    log10 :: Int -> Int
    log10 n = floor $ logBase (10 :: Double) (fromIntegral n)