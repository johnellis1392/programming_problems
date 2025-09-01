{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module AOC2020.Day4 (day, parse, part1, part2, Field (..), parser, valid, passport, field) where

import Common.Day (Day (Day))
import Common.Day qualified as D
import Common.Utils hiding (count)
import Common.Utils qualified as U
import Data.Bits ((.&.), (.|.))
import Data.Char (isDigit, isHexDigit)
import Data.Functor (($>), (<&>))
import Data.List (stripPrefix)
import Text.Parsec hiding (parse, try)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

day :: Day [Passport] Int
day =
  Day
    { D.day = "4",
      D.year = "2020",
      D.parse = parse,
      D.part1 = part1,
      D.part2 = part2
    }

data Field
  = ECL String
  | PID String
  | EYR String
  | HCL String
  | BYR String
  | IYR String
  | CID String
  | HGT String
  deriving (Show, Eq)

type Passport = [Field]

boundary :: Parser ()
boundary = eof <|> space $> ()

field :: Parser Field
field =
  choice
    [ fieldDesc "ecl:" ECL,
      fieldDesc "pid:" PID,
      fieldDesc "eyr:" EYR,
      fieldDesc "hcl:" HCL,
      fieldDesc "byr:" BYR,
      fieldDesc "iyr:" IYR,
      fieldDesc "cid:" CID,
      fieldDesc "hgt:" HGT
    ]
  where
    fieldDesc :: String -> (String -> Field) -> Parser Field
    fieldDesc s ctor = P.try $ string s >> manyTill anyChar (P.try (lookAhead boundary)) <&> ctor

passport :: Parser Passport
passport = do
  x <- field
  xs <- rest
  return (x : xs)
  where
    rest :: Parser [Field]
    rest =
      P.try ((lookAhead (string "\n\n") $> ()) <|> eof) $> []
        <|> do
          oneOf "\n "
          x <- field
          xs <- rest
          return (x : xs)

parser :: Parser [Passport]
parser = passport `sepBy` string "\n\n" <* eof

parse :: String -> [Passport]
parse = parseWith parser "(Day4)" . strip

encode :: Field -> Int
encode (ECL _) = 0b0000_0001
encode (PID _) = 0b0000_0010
encode (EYR _) = 0b0000_0100
encode (HCL _) = 0b0000_1000
encode (BYR _) = 0b0001_0000
encode (IYR _) = 0b0010_0000
encode (HGT _) = 0b0100_0000
encode (CID _) = 0b1000_0000

part1 :: [Passport] -> Int
part1 input =
  input
    <&> foldl (\a f -> a .|. encode f) 0
      |> U.count (\n -> n .&. 0b01111111 == 0b01111111)

part2 :: [Passport] -> Int
part2 input =
  input
    <&> foldl (\a f -> a .|. (if valid f then encode f else 0)) 0
      |> U.count (\n -> n .&. 0b01111111 == 0b01111111)

valid :: Field -> Bool
valid (ECL s) = s `elem` (["amb", "blu", "brn", "gry", "grn", "hzl", "oth"] :: [String])
valid (PID s) = fork ((== 9) . length) (&&) (all isDigit) s
valid (EYR s) = tryParseInt s <&> inRange (2020, 2030) |> orElseFalse
valid (HCL s) = stripPrefix "#" s <&> fork (all isHexDigit) (&&) ((== 6) . length) |> orElseFalse
valid (BYR s) = tryParseInt s <&> inRange (1920, 2002) |> orElseFalse
valid (IYR s) = tryParseInt s <&> inRange (2010, 2020) |> orElseFalse
valid (HGT s) = case s of
  (stripSuffix "in" -> Just s') -> inRange (59, 76) (parseInt s')
  (stripSuffix "cm" -> Just s') -> inRange (150, 193) (parseInt s')
  _ -> False
valid (CID _) = True

orElseFalse :: Maybe Bool -> Bool
orElseFalse = flip orElse False