{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module AOC2020.Day12 (parser, parse, part1, part2, day) where

import Common.Day qualified as D
import Common.Utils (parseWith, strip)
import Data.Functor (($>), (<&>))
import Text.Parsec hiding (State, parse)
import Text.Parsec.String

day :: D.Day Input Int
day =
  D.Day
    { D.day = "12",
      D.year = "2020",
      D.parse = parse,
      D.part1 = part1,
      D.part2 = part2
    }

type Input = [(Inst, Int)]

data Inst = N | S | E | W | L | R | F
  deriving (Eq, Show)

data Direction = North | South | East | West
  deriving (Eq, Show)

parser :: Parser Input
parser = inst `sepBy` newline <* eof
  where
    inst :: Parser (Inst, Int)
    inst = (,) <$> heading <*> (many1 digit <&> read @Int)

    heading :: Parser Inst
    heading =
      choice
        [ char 'N' $> N,
          char 'S' $> S,
          char 'E' $> E,
          char 'W' $> W,
          char 'L' $> L,
          char 'R' $> R,
          char 'F' $> F
        ]
        <?> "(N|S|E|W|L|R|F)"

parse :: String -> Input
parse = parseWith parser "Day12" . strip

type State = ((Int, Int), Direction)

part1 :: Input -> Int
part1 = go ((0, 0), East)
  where
    go :: State -> [(Inst, Int)] -> Int
    go ((x, y), _) [] = abs x + abs y
    go (p, d) ((N, n) : xs) = go (move p North n, d) xs
    go (p, d) ((S, n) : xs) = go (move p South n, d) xs
    go (p, d) ((E, n) : xs) = go (move p East n, d) xs
    go (p, d) ((W, n) : xs) = go (move p West n, d) xs
    go (p, d) ((R, n) : xs) = go (p, turn d R n) xs
    go (p, d) ((L, n) : xs) = go (p, turn d L n) xs
    go (p, d) ((F, n) : xs) = go (move p d n, d) xs

    move :: (Int, Int) -> Direction -> Int -> (Int, Int)
    move (x, y) North n = (x, y + n)
    move (x, y) South n = (x, y - n)
    move (x, y) East n = (x + n, y)
    move (x, y) West n = (x - n, y)

    turn :: Direction -> Inst -> Int -> Direction
    turn d _ 0 = d
    turn d i n | n < 0 = error $ "Illegal state: d=" ++ show d ++ ", i=" ++ show i ++ ", n=" ++ show n
    turn North L n = turn West L (n - 90)
    turn West L n = turn South L (n - 90)
    turn South L n = turn East L (n - 90)
    turn East L n = turn North L (n - 90)
    turn North R n = turn East R (n - 90)
    turn West R n = turn North R (n - 90)
    turn South R n = turn West R (n - 90)
    turn East R n = turn South R (n - 90)
    turn d i n = error $ "Illegal state: d=" ++ show d ++ ", i=" ++ show i ++ ", n=" ++ show n

part2 :: Input -> Int
part2 = go (0, 0) (10, 1)
  where
    go :: (Int, Int) -> (Int, Int) -> [(Inst, Int)] -> Int
    go (x, y) _ [] = abs x + abs y
    go ship (x, y) ((N, n) : insts) = go ship (x, y + n) insts
    go ship (x, y) ((S, n) : insts) = go ship (x, y - n) insts
    go ship (x, y) ((E, n) : insts) = go ship (x + n, y) insts
    go ship (x, y) ((W, n) : insts) = go ship (x - n, y) insts
    go ship waypoint ((L, n) : insts) = go ship (rotate waypoint (-1 * n)) insts
    go ship waypoint ((R, n) : insts) = go ship (rotate waypoint n) insts
    go ship waypoint ((F, n) : insts) = go (ship .+ (waypoint .* n)) waypoint insts

    (.*) :: (Int, Int) -> Int -> (Int, Int)
    (x, y) .* n = (x * n, y * n)

    (.+) :: (Int, Int) -> (Int, Int) -> (Int, Int)
    (x1, y1) .+ (x2, y2) = (x1 + x2, y1 + y2)

    -- mat2(cos t, -sin t,
    --      sin t,  cos t)
    -- \* vec2(x, y)
    rotate :: (Int, Int) -> Int -> (Int, Int)
    rotate (x, y) t =
      let tf = -((fromIntegral t :: Double) / 90.0) * (pi / 2.0)
          xf = fromIntegral x :: Double
          yf = fromIntegral y :: Double
          x' = (cos tf * xf) - (sin tf * yf)
          y' = (sin tf * xf) + (cos tf * yf)
       in (round x', round y')