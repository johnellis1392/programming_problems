{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module AOC2020.Day11 (parser, parse, part1, part2, day) where

import Common.Day qualified as D
import Common.Utils (parseWith, strip, (|>))
import Data.Array (Array, array, assocs, bounds, elems, inRange, (!))
import Data.Functor (($>), (<&>))
import Text.Parsec hiding (Empty, count, parse)
import Text.Parsec.String

day :: D.Day Input Int
day =
  D.Day
    { D.day = "11",
      D.year = "2020",
      D.parse = parse,
      D.part1 = part1,
      D.part2 = part2
    }

type Input = Array (Int, Int) Cell

data Cell
  = Floor
  | Occupied
  | Empty
  deriving (Eq, Show)

cell :: Parser Cell
cell = choice [char '.' $> Floor, char 'L' $> Empty] <?> "Cell"

parser :: Parser Input
parser =
  many1 cell `sepBy` newline <* eof <&> \rows ->
    let nRows = length rows
        nCols = length $ head rows
        values =
          zip [0 ..] rows >>= \(r, row) ->
            zip [0 ..] row <&> \(c, cell') -> ((r, c), cell')
     in array ((0, 0), (nRows - 1, nCols - 1)) values

parse :: String -> Input
parse = parseWith parser "Day11" . strip

directions :: [(Int, Int)]
directions = [(-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1)]

(.+) :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
(x1, y1) .+ (x2, y2) = (x1 + x2, y1 + y2)

neighbors :: Array (Int, Int) a -> (Int, Int) -> [((Int, Int), a)]
neighbors a p =
  fmap (p .+) directions
    |> filter (inRange . bounds $ a)
    |> fmap (\p' -> (p', a ! p'))

part1 :: Input -> Int
part1 curr =
  let curr' = array (bounds curr) . fmap update . assocs $ curr
   in if curr == curr'
        then length . filter (== Occupied) . elems $ curr'
        else part1 curr'
  where
    update :: ((Int, Int), Cell) -> ((Int, Int), Cell)
    update (p, Empty) | numOccupiedNeighbors p == 0 = (p, Occupied)
    update (p, Occupied) | numOccupiedNeighbors p >= 4 = (p, Empty)
    update (p, c) = (p, c)

    numOccupiedNeighbors :: (Int, Int) -> Int
    numOccupiedNeighbors p =
      neighbors curr p
        |> filter ((== Occupied) . snd)
        |> length

part2 :: Input -> Int
part2 curr =
  let curr' = array (bounds curr) . fmap update . assocs $ curr
   in if curr == curr'
        then length . filter (== Occupied) . elems $ curr'
        else part2 curr'
  where
    update :: ((Int, Int), Cell) -> ((Int, Int), Cell)
    update (p, Empty) | numOccupiedNeighbors p == 0 = (p, Occupied)
    update (p, Occupied) | numOccupiedNeighbors p >= 5 = (p, Empty)
    update (p, c) = (p, c)

    until' :: (a -> Bool) -> (a -> a) -> a -> a
    until' p f a
      | p a = a
      | otherwise = until' p f (f a)

    numOccupiedNeighbors :: (Int, Int) -> Int
    numOccupiedNeighbors p =
      directions
        <&> ( \d ->
                until' (\p' -> (not . inRange (bounds curr) $ p') || (curr ! p' == Occupied) || (curr ! p' == Empty)) (.+ d) (p .+ d)
            )
          |> filter (inRange (bounds curr))
          |> filter ((== Occupied) . (curr !))
          |> length
