{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module AOC2024.Day14 (parser, parse, part1, part2, day, part1') where

import Common.Day qualified as D
import Common.Utils (parseWith, strip)
import Control.Monad (liftM2)
import Data.Array (inRange)
import Data.Functor (($>))
import Data.HashSet qualified as S
import Text.Parsec hiding (parse)
import Text.Parsec.String

day :: D.Day Input Int
day =
  D.Day
    { D.day = "14",
      D.year = "2024",
      D.parse = parse,
      D.part1 = part1,
      D.part2 = part2
    }

type Point = (Int, Int)

type Vec = (Int, Int)

type Input = [(Point, Vec)]

parser :: Parser Input
parser = line `sepBy` newline
  where
    line = liftM2 (,) (string "p=" >> point) (space >> string "v=" >> point)
    point = (,) <$> number <*> (char ',' >> number)
    number = liftM2 (*) ((try (char '-') $> -1) <|> return 1) (read <$> many1 digit)

parse :: String -> Input
parse = parseWith parser "Day14" . strip

class Mul a b c | a b -> c where
  (.*) :: a -> b -> c

class Add a b c | a b -> c where
  (.+) :: a -> b -> c

class Mod a b c | a b -> c where
  (.%) :: a -> b -> c

instance (Add Point Vec Point) where
  (.+) :: Point -> Vec -> Point
  (x, y) .+ (dx, dy) = (x + dx, y + dy)

instance (Mul Vec Int Vec) where
  (.*) :: Vec -> Int -> Vec
  (dx, dy) .* t = (dx * t, dy * t)

instance (Mod Point (Int, Int) Point) where
  (.%) :: Point -> (Int, Int) -> Point
  (x, y) .% (mx, my) = (x', y')
    where
      x' = if x < 0 then (mx + x) - 1 else x `mod` mx
      y' = if y < 0 then (my + y) - 1 else y `mod` my

part1' :: Input -> (Int, Int) -> Int
part1' bots dim =
  let ps = fmap (\(p, v) -> (p .+ (v .* dt)) .% dim) bots
      qs = quads dim ps (replicate 4 S.empty)
   in product . fmap S.size $ qs
  where
    dt :: Int
    dt = 100

    quads :: (Int, Int) -> [Point] -> [S.HashSet Point] -> [S.HashSet Point]
    quads _ [] qs = qs
    quads (w, h) (p : ps) qs =
      case quad (w, h) p of
        Nothing -> quads (w, h) ps qs
        Just i -> take i qs ++ [S.insert p (qs !! i)] ++ drop (i + 1) qs

    quad :: (Int, Int) -> Point -> Maybe Int
    quad (w, h) (x, y)
      | inRange (0, w `div` 2) x && inRange (0, h `div` 2) y = Just 0
      | inRange ((w `div` 2) + 1, w - 1) x && inRange (0, h `div` 2) y = Just 1
      | inRange (0, w `div` 2) x && inRange ((h `div` 2) + 1, h) y = Just 0
      | inRange ((w `div` 2) + 1, w - 1) x && inRange ((h `div` 2) + 1, h) y = Just 0
      | otherwise = Nothing

part1 :: Input -> Int
part1 bots = part1' bots dim
  where
    dim :: (Int, Int)
    dim = (101, 103)

part2 :: Input -> Int
part2 _ = 0