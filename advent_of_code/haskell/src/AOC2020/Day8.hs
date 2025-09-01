{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module AOC2020.Day8 (parser, parse, part1, part2, day, Inst (..)) where

import Common.Day qualified as D
import Common.Utils (parseWith, strip)
import Data.Array (Array, bounds, listArray, (!), (//))
import Data.Either.Extra (fromLeft', fromRight')
import Data.Functor (($>), (<&>))
import Data.HashSet qualified as S
import Text.Parsec hiding (count, parse, (<|>))
import Text.Parsec qualified as P
import Text.Parsec.String

day :: D.Day Input Int
day =
  D.Day
    { D.day = "8",
      D.year = "2020",
      D.parse = parse,
      D.part1 = part1,
      D.part2 = part2
    }

type Input = Array Int Inst

data Inst where
  NOP :: Int -> Inst
  ACC :: Int -> Inst
  JMP :: Int -> Inst
  deriving (Show, Eq)

parser :: Parser Input
parser = (inst `sepBy` newline <* eof) <&> \insts -> listArray (0, length insts - 1) insts
  where
    inst :: Parser Inst
    inst =
      choice
        [ NOP <$> (string "nop" *> space *> int),
          ACC <$> (string "acc" *> space *> int),
          JMP <$> (string "jmp" *> space *> int)
        ]
        <?> "Illegal Instruction"

    int :: Parser Int
    int = (char '+' $> id P.<|> char '-' $> negate) <*> (many1 digit <&> (read @Int))

parse :: String -> Input
parse = parseWith parser "Day7" . strip

eval :: Array Int Inst -> Either Int Int
eval insts = go 0 0 S.empty
  where
    go :: Int -> Int -> S.HashSet Int -> Either Int Int
    go acc pc v
      | pc `S.member` v = Left acc
      | pc > (snd . bounds $ insts) = Right acc
      | otherwise = case insts ! pc of
          NOP _ -> go acc (pc + 1) (S.insert pc v)
          ACC x -> go (acc + x) (pc + 1) (S.insert pc v)
          JMP x -> go acc (pc + x) (S.insert pc v)

part1 :: Input -> Int
part1 insts = fromLeft' $ eval insts

part2 :: Input -> Int
part2 insts = fromRight' $ go 0
  where
    go :: Int -> Either Int Int
    go i =
      case insts ! i of
        NOP x -> eval (insts // [(i, JMP x)]) <|> go (i + 1)
        JMP x -> eval (insts // [(i, NOP x)]) <|> go (i + 1)
        ACC _ -> go (i + 1)

    (<|>) :: Either Int Int -> Either Int Int -> Either Int Int
    Right x <|> _ = Right x
    _ <|> o = o