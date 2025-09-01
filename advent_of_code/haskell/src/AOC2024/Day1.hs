module AOC2024.Day1 (day, parse, part1, part2) where

import Common.Day qualified as D
import Common.Utils
import Control.Arrow (second)
import Data.Functor ((<&>))
import Data.HashMap.Lazy hiding (foldl)
import Data.Hashable (Hashable)
import Data.List (sort)
import Data.Tuple.Extra (both)
import Text.Parsec hiding (parse)
import Text.Parsec qualified as P
import Text.ParserCombinators.Parsec hiding (parse)

day :: D.Day Input Int
day =
  D.Day
    { D.day = "1",
      D.year = "2024",
      D.parse = parse,
      D.part1 = part1,
      D.part2 = part2
    }

type Input = ([Int], [Int])

parse :: String -> Input
parse = unzip . unpack . P.parse parser "(Day1)" . strip
  where
    parser :: GenParser Char st [(Int, Int)]
    parser = line `sepBy` newline

    line :: GenParser Char st (Int, Int)
    line = do
      left <- many1 digit
      _ <- many1 space
      right <- many1 digit
      return (read @Int left, read @Int right)

    unpack :: Either ParseError [(Int, Int)] -> [(Int, Int)]
    unpack (Left err) = error $ "An error occurred while parsing: " ++ show err
    unpack (Right res) = res

part1 :: Input -> Int
part1 = sum . fmap (\(a, b) -> abs (a - b)) . uncurry zip . both sort

part2 :: Input -> Int
part2 input =
  second counts input
    |> uncurry similarities
    |> sum
  where
    counts :: (Hashable a) => [a] -> HashMap a Int
    counts = foldl (\m k -> insertWith (+) k 1 m) empty

    similarities :: [Int] -> HashMap Int Int -> [Int]
    similarities xs cs = xs <&> \x -> x * findWithDefault 0 x cs