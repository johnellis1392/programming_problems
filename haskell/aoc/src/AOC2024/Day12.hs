module AOC2024.Day12 (parser, parse, part1, part2, day) where

import Common.Day qualified as D
import Common.Utils (parseWith, strip, (|>))
import Data.Array (Array, array, bounds, inRange, indices, (!))
import Data.Functor ((<&>))
import Data.HashSet qualified as S
import Data.Tuple.Extra (both)
import Text.Parsec hiding (State, char, parse)
import Text.Parsec.String

day :: D.Day Input Int
day =
  D.Day
    { D.day = "12",
      D.year = "2024",
      D.parse = parse,
      D.part1 = part1,
      D.part2 = part2
    }

type Point = (Int, Int)

type Grid = Array Point Char

type Input = Grid

parser :: Parser Input
parser =
  (many1 upper `sepBy` newline <* eof) <&> \rows ->
    let nRows = length rows
        nCols = length (head rows)
        values =
          zip [0 ..] rows >>= \(r, row) ->
            zip [0 ..] row <&> \(c, char) -> ((r, c), char)
     in array ((0, 0), (nRows - 1, nCols - 1)) values

parse :: String -> Input
parse = parseWith parser "Day12" . strip

(.+) :: Point -> Point -> Point
(x1, y1) .+ (x2, y2) = (x1 + x2, y1 + y2)

(.-) :: Point -> Point -> Point
(x1, y1) .- (x2, y2) = (x1 - x2, y1 - y2)

adjacentTo :: Point -> Point -> Bool
adjacentTo p q = (1 ==) . uncurry (+) . both abs $ p .- q

directions :: [Point]
directions = [(1, 0), (-1, 0), (0, 1), (0, -1)]

(.&&.) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
f .&&. g = \x -> g x && f x

neighbors :: Point -> [Point]
neighbors p = fmap (.+ p) directions

flood :: Grid -> Char -> Point -> S.HashSet Point -> S.HashSet Point
flood g c p visited =
  let ns = filter ((not . flip S.member visited) .&&. ((c ==) . (g !)) .&&. inRange (bounds g)) . neighbors $ p
   in foldl (flip (flood g c)) (S.insert p visited) ns

groups :: Grid -> [Point] -> S.HashSet Point -> [S.HashSet Point]
groups _ [] _ = []
groups g (p : ps) visited
  | S.member p visited = groups g ps visited
  | otherwise =
      let c = g ! p
          group = flood g c p S.empty
          visited' = visited `S.union` group
       in group : groups g ps visited'

part1 :: Input -> Int
part1 g =
  let gs = groups g (indices g) S.empty
   in sum . fmap cost $ gs
  where
    perimeter :: S.HashSet Point -> Int
    perimeter s = length $ S.toList s >>= filter (not . flip S.member s) . neighbors

    area :: S.HashSet Point -> Int
    area = length

    cost :: S.HashSet Point -> Int
    cost ps = area ps * perimeter ps

part2 :: Input -> Int
part2 g =
  let gs = groups g (indices g) S.empty
   in sum . fmap cost $ gs
  where
    perimeter :: S.HashSet Point -> Int
    perimeter s =
      let ps =
            ( S.toList s
                |> fmap (\p -> (p, filter (not . flip S.member s) (neighbors p)))
                |> filter (not . null . snd)
            )
              >>= \(p, ns) -> fmap (p,) ns
          borderPoints = S.fromList ps
       in length $ collectWalls borderPoints ps S.empty
      where
        collectWalls :: S.HashSet (Point, Point) -> [(Point, Point)] -> S.HashSet (Point, Point) -> [S.HashSet (Point, Point)]
        collectWalls _ [] _ = []
        collectWalls borderPoints (p : ps) visited
          | p `S.member` visited = collectWalls borderPoints ps visited
          | otherwise =
              let wall = collectAdjacents borderPoints p visited
                  visited' = visited `S.union` wall
               in wall : collectWalls borderPoints ps visited'

        collectAdjacents :: S.HashSet (Point, Point) -> (Point, Point) -> S.HashSet (Point, Point) -> S.HashSet (Point, Point)
        collectAdjacents borderPoints (p, w) visited =
          let adjacents = S.filter (not . flip S.member visited) $ adjacentBorders borderPoints (p, w)
           in foldl (\s' (p', w') -> s' `S.union` collectAdjacents borderPoints (p', w') (S.insert (p, w) visited)) (S.singleton (p, w)) adjacents

        adjacentBorders :: S.HashSet (Point, Point) -> (Point, Point) -> S.HashSet (Point, Point)
        adjacentBorders borderPoints (p, w) =
          S.filter (\(p', w') -> (p, w) `adjacentTo'` (p', w')) borderPoints

        adjacentTo' :: (Point, Point) -> (Point, Point) -> Bool
        adjacentTo' (p, q) (p', q') = p `adjacentTo` p' && q `adjacentTo` q'

    area :: S.HashSet Point -> Int
    area = length

    cost :: S.HashSet Point -> Int
    cost ps = area ps * perimeter ps