{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}

module AOC2024.Day9 (parser, parse, part1, part2, day, stringify, expand, checksum, Cell (..), defrag) where

import Common.Day qualified as D
import Common.Utils (parseWith, strip)
import Control.Arrow (first)
import Data.Char (digitToInt, intToDigit)
import Data.Functor ((<&>))
import Text.Parsec (digit, many1)
import Text.Parsec.String

day :: D.Day Input Int
day =
  D.Day
    { D.day = "9",
      D.year = "2024",
      D.parse = parse,
      D.part1 = part1,
      D.part2 = part2
    }

type Input = [Int]

data Cell
  = Empty Int
  | Full Int Int
  deriving (Eq, Show)

parser :: Parser Input
parser = many1 digit <&> fmap digitToInt

parse :: String -> Input
parse = parseWith parser "Day9" . strip

expand :: [Int] -> [Cell]
expand = go 0
  where
    go :: Int -> [Int] -> [Cell]
    go _ [] = []
    go id' [x] = [Full id' x]
    go id' (x : y : zs) = [Full id' x, Empty y] ++ go (id' + 1) zs

stringify :: [Cell] -> String
stringify [] = []
stringify (Empty x : xs) = replicate x '.' ++ stringify xs
stringify (Full id' x : xs) = replicate x (intToDigit id') ++ stringify xs

defrag :: [Cell] -> [Cell]
defrag a =
  let (i, j) = (0, length a - 1)
   in go a (reverse a) (i, j)
  where
    go :: [Cell] -> [Cell] -> (Int, Int) -> [Cell]
    go [] _ _ = []
    go _ [] _ = []
    go _ ((Empty _) : _) (i, j) | i >= j = []
    go _ (y@(Full _ _) : _) (i, j) | i >= j = [y]
    go (x@(Full _ _) : xs) ys (i, j) = x : go xs ys (i + 1, j)
    go xs ((Empty _) : ys) (i, j) = go xs ys (i, j - 1)
    go ((Empty sx) : xs) ((Full id' sy) : ys) (i, j)
      | sy == 0 = go (Empty sx : xs) ys (i, j - 1)
      | sx == 0 = go xs ((Full id' sy) : ys) (i + 1, j)
      | sx == sy = (Full id' sx) : go xs ys (i + 1, j - 1)
      | sx > sy = (Full id' sy) : go (Empty (sx - sy) : xs) ys (i, j - 1)
      | otherwise = (Full id' sx) : go xs (Full id' (sy - sx) : ys) (i + 1, j)

checksum :: [Cell] -> Int
checksum = go 0
  where
    go :: Int -> [Cell] -> Int
    go _ [] = 0
    go i (Empty s : xs) = go (i + s) xs
    go i (Full id' s : xs) = f i id' s + go (i + s) xs

    f i id' s = sum $ zipWith (*) (iterate (+ 1) i) (replicate s id')

part1 :: Input -> Int
part1 = checksum . defrag . expand

cutLast :: [a] -> ([a], a)
cutLast [] = error "Cannot cut empty list"
cutLast [x] = ([], x)
cutLast (x : xs) = first (x :) $ cutLast xs

defrag2 :: [Cell] -> [Cell]
defrag2 a = go (cutLast a)
  where
    go :: ([Cell], Cell) -> [Cell]
    go ([], c) = [c]
    go (xs, c@(Empty _)) = go (cutLast xs) ++ [c]
    go (xs, c@(Full _ s)) =
      case tryInsert xs c of
        Nothing -> go (cutLast xs) ++ [c]
        Just xs' -> go (cutLast xs') ++ [Empty s]

    tryInsert :: [Cell] -> Cell -> Maybe [Cell]
    tryInsert [] _ = Nothing
    tryInsert _ (Empty _) = Nothing
    tryInsert (x@(Full _ _) : xs) c@(Full _ _) = tryInsert xs c <&> (x :)
    tryInsert (x@(Empty sx) : xs) c@(Full _ sc)
      | sx == sc = Just $ c : xs
      | sx < sc = tryInsert xs c <&> (x :)
      | otherwise =
          let x' = Empty (sx - sc)
           in Just $ c : x' : xs

part2 :: Input -> Int
part2 = checksum . defrag2 . expand

-- Old naive solution using brute-force list generation.
-- part1_OLD :: Input -> Int
-- part1_OLD =
--   sum
--     . zipWith (*) [0 ..]
--     . fromJust
--     . sequence
--     . takeWhile isJust
--     . elems
--     . defrag'
--     . translate'
--   where
--     expand' :: [Int] -> [Maybe Int]
--     expand' = go 0
--       where
--         go :: Int -> [Int] -> [Maybe Int]
--         go _ [] = []
--         go id' [x] = replicate x (Just id')
--         go id' (x : y : zs) = replicate x (Just id') ++ replicate y Nothing ++ go (id' + 1) zs

--     stringify' :: [Maybe Int] -> String
--     stringify' [] = []
--     stringify' (Nothing : xs) = '.' : stringify' xs
--     stringify' (Just x : xs) = intToDigit x : stringify' xs

--     translate' :: [Int] -> Array Int (Maybe Int)
--     translate' = fork ((0,) . (-1 +) . length) array (zip [0 ..]) . expand'

--     defrag' :: Array Int (Maybe Int) -> Array Int (Maybe Int)
--     defrag' a =
--       let (i, j) = bounds a
--        in go a (i, j)
--       where
--         go :: Array Int (Maybe Int) -> (Int, Int) -> Array Int (Maybe Int)
--         go a' (i, j)
--           | i >= j = a'
--           | isJust (a' ! i) = go a' (i + 1, j)
--           | isNothing (a' ! j) = go a' (i, j - 1)
--           | otherwise =
--               go (a' // [(i, a' ! j), (j, a' ! i)]) (i + 1, j - 1)