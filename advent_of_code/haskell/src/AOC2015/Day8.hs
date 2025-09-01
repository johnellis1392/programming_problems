module AOC2015.Day8 (part1, part2, parse, day, count1, count2) where

import Common.Day (Day (Day))
import Common.Day qualified as D
import Common.Utils (strip)
import Data.Char (isDigit)
import Data.Tuple.Extra (both, dupe, (***))

day :: Day [String] Int
day =
  Day
    { D.day = "8",
      D.year = "2015",
      D.parse = parse,
      D.part1 = part1,
      D.part2 = part2
    }

parse :: String -> [String]
parse = lines . strip

isHexDigit :: Char -> Bool
isHexDigit c | isDigit c = True
isHexDigit c | 'a' <= c && c <= 'f' = True
isHexDigit c | 'A' <= c && c <= 'F' = True
isHexDigit _ = False

count1 :: String -> Int
count1 [] = 0
count1 ('\\' : '"' : rest) = 1 + count1 rest
count1 ('"' : rest) = count1 rest
count1 ('\\' : 'x' : a : b : rest) | isHexDigit a && isHexDigit b = 1 + count1 rest
count1 ('\\' : '\\' : rest) = 1 + count1 rest
count1 (_ : rest) = 1 + count1 rest

part1 :: [String] -> Int
part1 = uncurry (-) . both sum . unzip . fmap ((length *** count1) . dupe)

count2 :: String -> Int
count2 = (+ 2) . go
  where
    go :: String -> Int
    go [] = 0
    go ('\\' : '"' : rest) = 4 + go rest
    go ('"' : rest) = 2 + go rest
    go ('\\' : 'x' : a : b : rest) | isHexDigit a && isHexDigit b = 5 + go rest
    go ('\\' : '\\' : rest) = 4 + go rest
    go (_ : rest) = 1 + go rest

part2 :: [String] -> Int
part2 = uncurry (-) . both sum . unzip . fmap ((count2 *** length) . dupe)