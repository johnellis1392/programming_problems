module AOC2020.Day6 (day, parse, part1, part2) where

import Common.Day qualified as D
import Common.Utils (parseWith, strip, (|>))
import Control.Monad (liftM2)
import Data.Functor (($>), (<&>))
import Data.HashSet qualified as HS
import Text.Parsec hiding (parse)
import Text.Parsec.String (Parser)

day :: D.Day [Group] Int
day =
  D.Day
    { D.day = "6",
      D.year = "2020",
      D.parse = parse,
      D.part1 = part1,
      D.part2 = part2
    }

type Group = [String]

lineBreak :: Parser ()
lineBreak = string "\n\n" $> ()

line :: Parser String
line = many1 letter

group :: Parser Group
group = liftM2 (:) line (manyTill (newline *> line) boundary)
  where
    boundary = try . lookAhead $ lineBreak <|> eof

parser :: Parser [Group]
parser = group `sepBy` lineBreak <* eof

parse :: String -> [Group]
parse = parseWith parser "Day6" . strip

part1 :: [Group] -> Int
part1 groups =
  groups
    <&> HS.size
      . foldl HS.union HS.empty
      . fmap HS.fromList
      |> sum

part2 :: [Group] -> Int
part2 groups =
  groups
    <&> HS.size
      . foldl1 HS.intersection
      . fmap HS.fromList
      |> sum
