{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module AOC2024.Day3 (day, mul, parser, do', dont, parse, part1, part2, Chunk (..)) where

import Common.Day (Day (Day))
import Common.Day qualified as D
import Common.Utils
import Control.Monad (liftM2)
import Data.Functor (($>))
import Text.Parsec hiding (parse)
import Text.Parsec.String (Parser)

day :: Day [Chunk] Int
day =
  Day
    { D.day = "3",
      D.year = "2024",
      D.parse = parse,
      D.part1 = part1,
      D.part2 = part2
    }

data Chunk
  = Mul Int Int
  | Do
  | Dont
  deriving (Show, Eq)

mul :: Parser Chunk
mul = do
  string "mul("
  x <- many1 digit
  char ','
  y <- many1 digit
  char ')'
  return $ Mul (read @Int x) (read @Int y)

do' :: Parser Chunk
do' = string "do()" $> Do

dont :: Parser Chunk
dont = string "don't()" $> Dont

parser :: Parser [Chunk]
parser =
  choice
    [ try (eof $> []),
      try mul >>= rest,
      try dont >>= rest,
      try do' >>= rest,
      anyChar *> parser
    ]
  where
    rest = flip (liftM2 (:)) parser . return

parse :: String -> [Chunk]
parse = parseWith parser "Day3"

part1 :: [Chunk] -> Int
part1 =
  filter
    ( \case
        Mul _ _ -> True
        _ -> False
    )
    >.> fmap (\(Mul x y) -> x * y)
    >.> sum

part2 :: [Chunk] -> Int
part2 = go True 0
  where
    go :: Bool -> Int -> [Chunk] -> Int
    go _ n [] = n
    go True n (Mul x y : xs) = go True (n + x * y) xs
    go True n (Dont : xs) = go False n xs
    go False n (Do : xs) = go True n xs
    go enabled n (_ : xs) = go enabled n xs