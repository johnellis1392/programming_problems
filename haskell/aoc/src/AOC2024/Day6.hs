{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module AOC2024.Day6 (parser, parse, part1, part2, day) where

import Common.Day qualified as D
import Common.Grid (get')
import Common.Grid qualified as G
import Common.Points (Direction (..), Vertex2i, moverc, turnrv)
import Common.Utils (parseWith, strip)
import Data.Functor (($>))
import Data.HashSet qualified as HS
import Data.List (nub)
import Data.Maybe (fromJust)
import Text.Parsec hiding (count, parse)
import Text.Parsec.String

day :: D.Day Input Int
day =
  D.Day
    { D.day = "6",
      D.year = "2024",
      D.parse = parse,
      D.part1 = part1,
      D.part2 = part2
    }

data Cell = Open | Obstruction | Origin
  deriving (Eq, Show)

type Input = (Vertex2i, G.Grid Cell)

cell :: Parser Cell
cell =
  choice
    [ char '.' $> Open,
      char '#' $> Obstruction,
      char '^' $> Origin
    ]
    <?> "Illegal Cell"

parser :: Parser Input
parser = do
  rows <- many1 cell `sepBy` newline
  eof
  let g = G.makeGrid rows
  let start = (fst . fromJust $ G.find g (== Origin), North)
  return (start, g)

parse :: String -> Input
parse = parseWith parser "Day6" . strip

until' :: (a -> Maybe a) -> a -> [a]
until' f x = case f x of
  Just x' -> x' : until' f x'
  Nothing -> []

calcPath :: G.Grid Cell -> Vertex2i -> [Vertex2i]
calcPath g v = v : until' step v
  where
    step :: Vertex2i -> Maybe Vertex2i
    step v'@(p, d) =
      let p' = moverc p d
       in case g `get'` p' of
            Nothing -> Nothing
            Just Obstruction -> Just (turnrv v')
            Just _ -> Just (p', d)

part1 :: Input -> Int
part1 (start, g) = HS.size . HS.fromList . fmap fst . calcPath g $ start

part2 :: Input -> Int
part2 (start, g) =
  length
    . filter (\p -> isLoop HS.empty $ calcPath (G.set g p Obstruction) start)
    . nub
    . fmap fst
    . drop 1
    $ calcPath g start
  where
    isLoop :: HS.HashSet Vertex2i -> [Vertex2i] -> Bool
    isLoop _ [] = False
    isLoop h (v : vs) = v `HS.member` h || isLoop (HS.insert v h) vs
