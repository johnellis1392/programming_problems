module AOC2020.Day9 (parser, parse, part1, part2, day) where

import Common.Day qualified as D
import Common.Utils (combinations, fork, nwise, parseWith, strip)
import Data.Functor ((<&>))
import Text.Parsec hiding (Empty, count, parse)
import Text.Parsec.String

day :: D.Day Input Int
day =
  D.Day
    { D.day = "9",
      D.year = "2020",
      D.parse = parse,
      D.part1 = part1,
      D.part2 = part2
    }

type Input = (Int, [Int])

defaultPreamble :: Int
defaultPreamble = 25

parser :: Parser Input
parser = (many1 digit `sepBy` newline) <* eof <&> (defaultPreamble,) . fmap (read @Int)

parse :: String -> Input
parse = parseWith parser "Day9" . strip

findInvalid :: Int -> [Int] -> Int
findInvalid preamble xs =
  let preambles = nwise preamble xs
      xs' = zip (drop preamble xs) preambles
   in fst . head . filter (not . valid) $ xs'
  where
    valid :: (Int, [Int]) -> Bool
    valid (y, ys) = any ((== y) . sum) . combinations 2 $ ys

part1 :: Input -> Int
part1 (preamble, xs) = findInvalid preamble xs

part2 :: Input -> Int
part2 (preamble, vs) =
  let n = findInvalid preamble vs
      ys = go n (take 2 vs) (drop 2 vs)
   in fork minimum (+) maximum ys
  where
    go :: Int -> [Int] -> [Int] -> [Int]
    go _ _ [] = error "Could not find range"
    go n xs ys
      | sum xs == n = xs
      | length xs == 2 = go n (xs ++ [head ys]) (tail ys)
      | sum xs > n = go n (tail xs) ys
      | otherwise = go n (xs ++ [head ys]) (tail ys)