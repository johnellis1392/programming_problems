{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module AOC2024.Day10 (parser, parse, part1, part2, day) where

import Common.Day qualified as D
import Common.Utils (parseWith, strip)
import Data.Array (Array, Ix (..), array, assocs, bounds, inRange, (!))
import Data.Char (digitToInt)
import Data.Functor ((<&>))
import Data.List (nub)
import Text.Parsec hiding (State, parse)
import Text.Parsec.String

day :: D.Day Input Int
day =
  D.Day
    { D.day = "10",
      D.year = "2024",
      D.parse = parse,
      D.part1 = part1,
      D.part2 = part2
    }

type Grid = Array (Int, Int) Int

type Input = Grid

parser :: Parser Input
parser =
  (many1 digit `sepBy` newline <* eof)
    <&> ( \rows ->
            let nRows = length rows
                nCols = length (head rows)
                values =
                  zip [0 ..] rows >>= \(r, row) ->
                    zip [0 ..] row <&> \(c, v) ->
                      ((r, c), v)
             in array ((0, 0), (nRows - 1, nCols - 1)) values
        )
      . fmap (fmap digitToInt)

parse :: String -> Input
parse = parseWith parser "Day10" . strip

(.+) :: (Int, Int) -> (Int, Int) -> (Int, Int)
(a, b) .+ (c, d) = (a + c, b + d)

contains :: (Ix a) => Array a e -> a -> Bool
contains a = inRange (bounds a)

directions :: [(Int, Int)]
directions = [(1, 0), (-1, 0), (0, 1), (0, -1)]

neighbors :: Grid -> (Int, Int) -> [(Int, Int)]
neighbors g p = filter (contains g) $ directions <&> (p .+)

step :: Grid -> Int -> (Int, Int) -> [(Int, Int)]
step g v = filter (\p' -> g ! p' == v) . neighbors g

trailheads :: Grid -> [(Int, Int)]
trailheads = fmap fst . filter ((== 0) . snd) . assocs

part1 :: Input -> Int
part1 g = sum . fmap rank . trailheads $ g
  where
    rank :: (Int, Int) -> Int
    rank p = length $ foldl (\ps v -> nub $ ps >>= step g v) [p] [1 .. 9]

part2 :: Input -> Int
part2 g = sum . fmap rating . trailheads $ g
  where
    rating :: (Int, Int) -> Int
    rating p = length $ foldl (\ps v -> ps >>= step g v) [p] [1 .. 9]