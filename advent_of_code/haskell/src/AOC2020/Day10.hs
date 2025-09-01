{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module AOC2020.Day10 (parser, parse, part1, part2, day) where

import Common.Day qualified as D
import Common.Utils (fork, nwise, parseWith, strip, (|>))
import Data.Functor ((<&>))
import Data.List (sort)
import Data.List.Extra (partition)
import Data.Tuple.Extra (both)
import Text.Parsec hiding (Empty, count, parse)
import Text.Parsec.String

day :: D.Day Input Int
day =
  D.Day
    { D.day = "10",
      D.year = "2020",
      D.parse = parse,
      D.part1 = part1,
      D.part2 = part2
    }

type Input = [Int]

parser :: Parser Input
parser = many1 digit `sepBy` newline <* eof <&> fork (0 :) (++) ((: []) . (+ 3) . maximum) . sort . fmap (read @Int)

parse :: String -> Input
parse = parseWith parser "Day10" . strip

part1 :: Input -> Int
part1 vs =
  nwise 2 vs
    |> fmap (\[x, y] -> y - x)
    |> filter (fork (== 1) (||) (== 3))
    |> partition (== 1)
    |> both length
    |> uncurry (*)

part2 :: Input -> Int
part2 vs =
  let vs' = Just 1 : drop 1 (mkList 0 vs)
   in go vs'
  where
    go :: [Maybe Int] -> Int
    go [Just x] = x
    go (Nothing : xs) = go xs
    go (Just x : xs) =
      let xs' = fmap (fmap (+ x)) (take 3 xs) ++ drop 3 xs
       in go xs'
    go _ = undefined

    mkList :: Int -> [Int] -> [Maybe Int]
    mkList _ [] = []
    mkList i (x : xs)
      | i == x = Just 0 : mkList (i + 1) xs
      | otherwise = Nothing : mkList (i + 1) (x : xs)