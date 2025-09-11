{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module AOC2020.Day7 (day, parse, parser, part1, part2, Bag, bag, bagList, bagEntry, bagName) where

import Common.Day qualified as D
import Common.Utils (parseWith, strip)
import Data.Functor (($>), (<&>))
import Data.HashMap.Lazy qualified as HM
import Data.HashSet qualified as HS
import Text.Parsec hiding (parse)
import Text.Parsec.String

day :: D.Day Input Int
day =
  D.Day
    { D.day = "7",
      D.year = "2020",
      D.parse = parse,
      D.part1 = part1,
      D.part2 = part2
    }

type Bag = (String, [(String, Int)])

type Input = [Bag]

bagName :: Parser String
bagName = do
  x <- many1 letter
  space
  y <- many1 letter
  return $ x ++ " " ++ y

bagEntry :: Parser (String, Int)
bagEntry = do
  n <- many1 digit
  space
  name <- bagName
  space
  string "bag" >> optional (char 's')
  return (name, read @Int n)

bagList :: Parser [(String, Int)]
bagList = (string "no other bags." $> []) <|> bagEntry `sepBy` string ", " <* char '.'

bag :: Parser Bag
bag = do
  n <- bagName
  string " bags contain "
  bs <- bagList <|> (string "no other bags." $> [])
  return (n, bs)

parser :: Parser Input
parser = bag `sepBy` newline

parse :: String -> Input
parse = parseWith parser "Day7" . strip

part1 :: [Bag] -> Int
part1 bags =
  let invertedMap = invert bags
   in HS.size . HS.delete target . walk invertedMap $ target
  where
    target :: String
    target = "shiny gold"

    invert :: [Bag] -> HM.HashMap String [String]
    invert bags' =
      let entries = bags' >>= (\(name, deps) -> deps <&> (,[name]) . fst)
       in HM.fromListWith (++) entries

    walk :: HM.HashMap String [String] -> String -> HS.HashSet String
    walk m name = case m HM.!? name of
      Nothing -> HS.singleton name
      Just names -> HS.union (HS.singleton name) $ foldl HS.union HS.empty $ fmap (walk m) names

part2 :: [Bag] -> Int
part2 bags =
  let m = HM.fromList bags
   in walk m target - 1
  where
    target :: String
    target = "shiny gold"

    walk :: HM.HashMap String [(String, Int)] -> String -> Int
    walk m name = (+ 1) . sum . fmap (\(name', n) -> n * walk m name') $ m HM.! name