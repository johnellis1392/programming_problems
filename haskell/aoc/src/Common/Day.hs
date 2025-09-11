module Common.Day (Day (..)) where

data Day input output = Day
  { day :: String,
    year :: String,
    parse :: String -> input,
    part1 :: input -> output,
    part2 :: input -> output
  }