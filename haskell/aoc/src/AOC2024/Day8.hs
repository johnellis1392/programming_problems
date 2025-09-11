{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module AOC2024.Day8 (parser, parse, part1, part2, day, Cell (..), Freq, Input, antinodePair) where

import Common.Day qualified as D
import Common.Utils (combinations, groupWith, parseWith, strip, (|>))
import Data.Array (Array, array, assocs, bounds, inRange)
import Data.Functor (($>), (<&>))
import Data.HashMap.Lazy qualified as M
import Data.HashSet qualified as S
import Text.Parsec hiding (Empty, count, parse)
import Text.Parsec.String

day :: D.Day Input Int
day =
  D.Day
    { D.day = "8",
      D.year = "2024",
      D.parse = parse,
      D.part1 = part1,
      D.part2 = part2
    }

type Freq = Char

data Cell
  = Empty
  | Signal Freq

type Input = Array (Int, Int) Cell

cell :: Parser Cell
cell = (char '.' $> Empty) <|> (Signal <$> alphaNum)

parser :: Parser Input
parser =
  ((many1 cell `sepBy` newline) <* eof) <&> \rows ->
    let nRows = length rows
        nCols = length (head rows)
        values =
          zip [0 ..] rows >>= \(y, row) ->
            zip [0 ..] row <&> \(x, v) -> ((x, y), v)
     in array ((0, 0), (nCols - 1, nRows - 1)) values

parse :: String -> Input
parse = parseWith parser "Day8" . strip

isSignal :: Cell -> Bool
isSignal (Signal _) = True
isSignal Empty = False

freq :: Cell -> Freq
freq (Signal f) = f
freq _ = error "Empty cells don't have frequencies"

(.-) :: (Num a) => (a, a) -> (a, a) -> (a, a)
(x1, y1) .- (x2, y2) = (x1 - x2, y1 - y2)

(.*) :: (Num b) => b -> (b, b) -> (b, b)
n .* (x1, y1) = (n * x1, n * y1)

(.+) :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
(x1, y1) .+ (x2, y2) = (x1 + x2, y1 + y2)

antinodePair :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
antinodePair p1 p2 = [p3, p4]
  where
    p3 = (2 .* (p2 .- p1)) .+ p1
    p4 = (2 .* (p1 .- p2)) .+ p2

genAntinodes :: Array (Int, Int) Cell -> ((Int, Int) -> (Int, Int) -> [(Int, Int)]) -> S.HashSet (Int, Int)
genAntinodes a calcAntinodes =
  assocs a
    |> filter (isSignal . snd)
    |> groupWith (freq . snd)
    |> M.map (fmap fst)
    |> M.map calcAntinodes'
    |> M.elems
    |> S.unions
  where
    calcAntinodes' :: [(Int, Int)] -> S.HashSet (Int, Int)
    calcAntinodes' ps =
      combinations 2 ps
        >>= (\[x, y] -> calcAntinodes x y)
          |> filter (inRange $ bounds a)
          |> S.fromList

part1 :: Input -> Int
part1 a = S.size $ genAntinodes a antinodePair

part2 :: Input -> Int
part2 a = S.size $ genAntinodes a antinodeSlope
  where
    until' :: (a -> Bool) -> (a -> a) -> a -> [a]
    until' pred' f a'
      | pred' a' = []
      | otherwise = a' : until' pred' f (f a')

    antinodeSlope :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
    antinodeSlope p1 p2 =
      let slope p v = until' (not . inRange (bounds a)) (.+ v) p
          r1 = slope p1 (p2 .- p1)
          r2 = slope p2 (p1 .- p2)
       in r1 ++ r2