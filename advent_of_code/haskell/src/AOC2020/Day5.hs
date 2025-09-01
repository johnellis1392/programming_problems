module AOC2020.Day5 (day, parse, part1, part2, seatAssignment, seatId, Seat (..), Partition, Cmd (..), Row, Col) where

import Common.Day (Day (Day))
import Common.Day qualified as D
import Common.Utils hiding (count)
import Control.Arrow ((***))
import Data.Functor (($>))
import Data.List (sort)
import Text.Parsec hiding (parse)
import Text.Parsec.String (Parser)

day :: Day [Partition] Int
day =
  Day
    { D.day = "5",
      D.year = "2020",
      D.parse = parse,
      D.part1 = part1,
      D.part2 = part2
    }

data Cmd = F | B | L | R
  deriving (Eq, Show)

type Partition = [Cmd]

type Row = Int

type Col = Int

data Seat = Seat Row Col
  deriving (Show, Eq)

cmd :: Parser Cmd
cmd =
  choice
    [ try (char 'F') $> F,
      try (char 'B') $> B,
      try (char 'L') $> L,
      try (char 'R') $> R
    ]

partition :: Parser Partition
partition = count 10 cmd

parser :: Parser [Partition]
parser = do
  x <- partition
  xs <- manyTill (newline *> partition) (try eof)
  return (x : xs)

parse :: String -> [Partition]
parse = parseWith parser "Day5" . strip

type Range a = (a, a)

seatAssignment :: Partition -> Seat
seatAssignment = uncurry Seat . (fst *** fst) . foldl f (defaultRows, defaultCols)
  where
    defaultRows :: Range Row
    defaultRows = (0, 128)

    defaultCols :: Range Col
    defaultCols = (0, 8)

    f :: (Range Row, Range Col) -> Cmd -> (Range Row, Range Col)
    f ((r0, r1), cs) F = ((r0, (r0 + r1) `div` 2), cs)
    f ((r0, r1), cs) B = (((r0 + r1) `div` 2, r1), cs)
    f (rs, (c0, c1)) L = (rs, (c0, (c0 + c1) `div` 2))
    f (rs, (c0, c1)) R = (rs, ((c0 + c1) `div` 2, c1))

seatId :: Seat -> Int
seatId (Seat row col) = row * 8 + col

part1 :: [Partition] -> Int
part1 = maximum . fmap (seatId . seatAssignment)

part2 :: [Partition] -> Int
part2 =
  fmap (seatId . seatAssignment)
    >.> sort
    >.> walk
    >.> head
  where
    walk :: [Int] -> [Int]
    walk [] = []
    walk [_] = []
    walk (x : y : ys)
      | (y - x) > 1 = (x + 1) : walk ys
      | otherwise = walk ys