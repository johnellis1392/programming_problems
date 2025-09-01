module AOC2024.Day2 (day, parse, part1, part2, Report, Level, Input) where

import Common.Day qualified as D
import Common.Utils
import Data.Either.Extra (fromRight')
import Data.Functor ((<&>))
import Text.Parsec hiding (count, parse)
import Text.Parsec qualified as Parsec
import Text.ParserCombinators.Parsec hiding (count, parse)

day :: D.Day Input Int
day =
  D.Day
    { D.day = "2",
      D.year = "2024",
      D.parse = parse,
      D.part1 = part1,
      D.part2 = part2
    }

type Level = Int

type Report = [Level]

type Input = [Report]

parse :: String -> Input
parse = fromRight' . Parsec.parse (report `sepBy` newline) "Day2" . strip
  where
    report :: GenParser Char st Report
    report = (many1 digit `sepBy` many1 (char ' ')) <&> map (read @Int)

valid :: Report -> Bool
valid =
  fork
    (all (fork (>= 1) (&&) (<= 3)))
    (||)
    (all (fork (>= -3) (&&) (<= -1)))
    . fork (drop 1) (zipWith (-)) id

part1 :: Input -> Int
part1 = count valid

part2 :: Input -> Int
part2 = count $ any valid . fork ((-1 +) . length) combinations id