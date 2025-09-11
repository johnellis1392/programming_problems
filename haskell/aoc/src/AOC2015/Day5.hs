module AOC2015.Day5 (part1, part2, nice, nicer, parse, day) where

import Common.Day (Day (Day))
import Common.Day qualified as D
import Data.List (isPrefixOf)

day :: Day [String] Int
day =
  Day
    { D.day = "5",
      D.year = "2015",
      D.parse = parse,
      D.part1 = part1,
      D.part2 = part2
    }

parse :: String -> [String]
parse = lines

nice :: String -> Bool
nice s = check1 s && check2 s && check3 s
  where
    check1 = (>= 3) . length . filter isVowel
    isVowel 'a' = True
    isVowel 'e' = True
    isVowel 'i' = True
    isVowel 'o' = True
    isVowel 'u' = True
    isVowel _ = False

    check2 [] = False
    check2 [_] = False
    check2 (a : b : _) | a == b = True
    check2 (_ : rest) = check2 rest

    check3 [] = True
    check3 [_] = True
    check3 ('a' : 'b' : _) = False
    check3 ('c' : 'd' : _) = False
    check3 ('p' : 'q' : _) = False
    check3 ('x' : 'y' : _) = False
    check3 (_ : rest) = check3 rest

part1 :: [String] -> Int
part1 = length . filter nice

find :: (Eq a) => [a] -> [a] -> Maybe Int
find sep str = go str 0
  where
    go [] _ = Nothing
    go s i
      | sep `isPrefixOf` s = Just i
      | otherwise = go (tail s) (i + 1)

nicer :: String -> Bool
nicer s = check1 s && check2 s
  where
    check1 [] = False
    check1 [_] = False
    check1 [_, _] = False
    check1 (a : b : rest) = case find [a, b] rest of
      Just _ -> True
      Nothing -> check1 (b : rest)

    check2 (a : b : c : rest)
      | a == c = True
      | otherwise = check2 (b : c : rest)
    check2 _ = False

part2 :: [String] -> Int
part2 = length . filter nicer