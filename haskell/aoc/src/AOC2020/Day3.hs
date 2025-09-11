module AOC2020.Day3 (day, parse, part1, part2) where

import Common.Day (Day (Day))
import Common.Day qualified as D
import Common.Utils ((|>))
import Data.Functor ((<&>))
import Data.List.Extra (trim)

day :: Day (Grid Cell) Int
day =
  Day
    { D.day = "3",
      D.year = "2020",
      D.parse = parse,
      D.part1 = part1,
      D.part2 = part2
    }

type Point = (Int, Int)

type Slope = (Int, Int)

(.+) :: Point -> Point -> Point
(x1, y1) .+ (x2, y2) = (x1 + x2, y1 + y2)

data Cell = Tree | Open

type Grid a = [[a]]

get :: Grid a -> Point -> a
grid `get` (c, r) = (grid !! r) !! c

parse :: String -> Grid Cell
parse s = trim s |> lines <&> fmap toCell
  where
    toCell :: Char -> Cell
    toCell c = case c of
      '.' -> Open
      '#' -> Tree
      _ -> error $ "Illegal character: " ++ show c

part1 :: Grid Cell -> Int
part1 grid = go origin 0
  where
    go :: Point -> Int -> Int
    go p@(c, r) trees
      | r >= rows = trees
      | otherwise = go (p .+ slope) (trees + dt)
      where
        dt = case grid `get` (c `mod` cols, r) of
          Open -> 0
          Tree -> 1
    rows = length grid
    cols = length $ head grid
    slope = (3, 1) :: Slope
    origin = (0, 0)

part2 :: Grid Cell -> Int
part2 grid = product . fmap (go origin 0) $ slopes
  where
    go :: Point -> Int -> Slope -> Int
    go p@(c, r) trees slope
      | r >= rows = trees
      | otherwise = go (p .+ slope) (trees + dt) slope
      where
        dt = case grid `get` (c `mod` cols, r) of
          Open -> 0
          Tree -> 1

    slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)] :: [Slope]
    rows = length grid
    cols = length $ head grid
    origin = (0, 0)