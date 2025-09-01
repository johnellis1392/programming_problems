module AOC2024.Day4 (day, parse, part1, part2, parser) where

import Common.Day qualified as D
import Common.Grid
import Common.Points (Direction (..), Point2i, move)
import Common.Utils
import Data.Functor ((<&>))
import Data.List (sort)
import Data.Tuple.Extra (both)
import Text.Parsec hiding (parse)
import Text.Parsec.String

day :: D.Day (Grid Cell) Int
day =
  D.Day
    { D.day = "4",
      D.year = "2024",
      D.parse = parse,
      D.part1 = part1,
      D.part2 = part2
    }

data Cell = X | M | A | S
  deriving (Show, Eq, Ord)

parser :: Parser (Grid Cell)
parser = do
  rows <- many (oneOf "XMAS") `sepBy` newline <* eof
  return $ makeGrid (rows <&> fmap encode)
  where
    encode 'X' = X
    encode 'M' = M
    encode 'A' = A
    encode 'S' = S
    encode c = error $ "Illegal Input: " ++ show c

parse :: String -> Grid Cell
parse = parseWith parser "Day4" . strip

part1 :: Grid Cell -> Int
part1 g =
  (pointSet g |> filter ((X ==) . get g))
    >>= (\p -> fmap (p,) directions)
    >>= tryStep M
    >>= tryStep A
    >>= tryStep S
      |> length
  where
    directions :: [[Direction]]
    directions = [[North], [South], [East], [West], [North, West], [North, East], [South, West], [South, East]]

    move' :: Point2i -> [Direction] -> Point2i
    move' = foldl move

    tryStep :: Cell -> (Point2i, [Direction]) -> [(Point2i, [Direction])]
    tryStep c (p, d) =
      let p' = move' p d
       in [(p', d) | g `contains` p' && g `get` p' == c]

part2 :: Grid Cell -> Int
part2 g =
  (pointSet g |> filter ((A ==) . get g))
    <&> (both (fmap (get g) . filter (contains g)) . xs)
      |> filter (uncurry (&&) . both ((==) [M, A, S] . sort))
      |> length
  where
    ne = [North, East]
    nw = [North, West]
    se = [South, East]
    sw = [South, West]

    xs :: Point2i -> ([Point2i], [Point2i])
    xs p = ([move' p ne, p, move' p sw], [move' p nw, p, move' p se])

    move' :: Point2i -> [Direction] -> Point2i
    move' = foldl move