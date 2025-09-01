module AOC2024.Day13 (parser, parse, part1, part2, day, toMat, (.*), det, sle, sled, cost, invd, invi) where

import Common.Day qualified as D
import Common.Utils (parseWith, strip)
import Control.Monad (liftM2)
import Data.Functor ((<&>))
import Data.Maybe (mapMaybe)
import Data.Tuple.Extra (both)
import Text.Parsec hiding (parse)
import Text.Parsec.String

day :: D.Day Input N
day =
  D.Day
    { D.day = "13",
      D.year = "2024",
      D.parse = parse,
      D.part1 = part1,
      D.part2 = part2
    }

type Vec2 a = (a, a)

type Mat2 a = ((a, a), (a, a))

type Button a = Vec2 a

type Prize a = Vec2 a

type Machine a = (Button a, Button a, Prize a)

type N = Integer

type Input = [Machine N]

number :: Parser N
number = read <$> many1 digit

machine :: Parser (Machine N)
machine = do
  a <- liftM2 (,) (string "Button A: X+" *> number) (string ", Y+" *> number)
  newline
  b <- liftM2 (,) (string "Button B: X+" *> number) (string ", Y+" *> number)
  newline
  p <- liftM2 (,) (string "Prize: X=" *> number) (string ", Y=" *> number)
  return (a, b, p)

parser :: Parser Input
parser = machine `sepBy` string "\n\n" <* eof

parse :: String -> Input
parse = parseWith parser "Day13" . strip

det :: (Num a) => Mat2 a -> a
det ((a, b), (c, d)) = a * d - b * c

invd :: (Fractional a, Eq a) => Mat2 a -> Maybe (Mat2 a)
invd m@((a, b), (c, d)) =
  let d' = det m
   in if d' == 0
        then Nothing
        else Just ((d / d', -b / d'), (-c / d', a / d'))

invi :: (Integral a) => Mat2 a -> Maybe (Mat2 a)
invi m@((a, b), (c, d)) =
  let d' = det m
   in if d == 0
        then Nothing
        else Just ((d `div` d', -b `div` d'), (-c `div` d', a `div` d'))

(.*) :: (Num a) => Mat2 a -> Vec2 a -> Vec2 a
((a, b), (c, d)) .* (x, y) = (a * x + b * y, c * x + d * y)

toMat :: Button a -> Button a -> Mat2 a
toMat (xa, ya) (xb, yb) = ((xa, xb), (ya, yb))

-- System of Linear Equations: Solve a system of linear
-- equations of the form:
-- [xa xb][na] = [px]
-- [ya yb][nb]   [py]
sled :: (Fractional a, Eq a) => Mat2 a -> Vec2 a -> Maybe (Vec2 a)
sled m v = invd m <&> (.* v)

sle :: (Integral a) => Mat2 a -> Vec2 a -> Maybe (Vec2 a)
sle m@((a, b), (c, d)) v =
  let d' = det m
      m' = ((d, -b), (-c, a))
      (x, y) = m' .* v
      v'@(x', y') = (x `div` d', y `div` d')
   in (if (d' == 0) || (m .* v' /= v) then Nothing else Just (x', y'))

cost :: Machine N -> Maybe N
cost (a, b, p) =
  ( sle (toMat a b) p >>= (\v -> if (uncurry (&&) . both (<= 100)) v then return v else Nothing)
  )
    <&> (\(an, bn) -> 3 * an + bn)

-- 31265 too high
-- 31236 too high
part1 :: Input -> N
part1 = sum . mapMaybe cost

cost' :: Machine N -> Maybe N
cost' (a, b, p) = sle (toMat a b) p <&> \(an, bn) -> 3 * an + bn

part2 :: Input -> N
part2 = sum . mapMaybe (cost' . f)
  where
    offset :: N
    offset = 10000000000000

    f :: Machine N -> Machine N
    f (a, b, (px, py)) = (a, b, (px + offset, py + offset))