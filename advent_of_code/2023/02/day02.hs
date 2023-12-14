{-# LANGUAGE BangPatterns #-}

import Data.Char (isSpace)
import Data.List (isPrefixOf)
import Data.Maybe (maybeToList)
import Data.Functor ((<&>))

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

indexOf :: Eq a => [a] -> a -> Maybe Int
indexOf xs x = go 0 xs
  where 
    go _ [] = Nothing
    go i (h:_) | h == x = Just i
    go i (_:rest) = go (i + 1) rest

findString :: String -> String -> Maybe Int
findString input target = go 0 input
  where
    go _ [] = Nothing
    go i s | target `isPrefixOf` s = Just i
    go i (_:ss) = go (i + 1) ss

split1 :: String -> String -> Maybe (String, String)
split1 str sep = 
  case findString str sep of 
    Nothing -> Nothing
    Just i -> Just (take i str, drop (i + length sep) str)

split :: String -> String -> [String]
split "" _ = []
split str sep =
  case split1 str sep of
    Nothing -> [str]
    Just (s1, s2) -> s1 : split s2 sep

parseInt :: String -> Int
parseInt s = read s :: Int

data Color = Red | Green | Blue deriving (Show)
data Roll = Roll Int Color deriving (Show)
newtype Round = Round [Roll] deriving (Show)
data Game = Game Int [Round] deriving (Show)

parseColor :: String -> Maybe Color
parseColor "red" = Just Red
parseColor "green" = Just Green
parseColor "blue" = Just Blue
parseColor _ = Nothing

parseRoll :: String -> Maybe Roll
parseRoll s = split1 s " " >>= \(n, color) -> 
  case parseColor color of
    Nothing -> Nothing
    Just c -> Just (Roll (read n) c)

parseRound :: String -> Maybe Round
parseRound s = sequence (split s ", " <&> parseRoll) <&> Round

parseGame :: String -> Maybe Game
parseGame s = do
  let !_ = putStrLn "Check"
  (ids, rounds) <- split1 s ": "
  let !_ = putStrLn ids
  let !_ = putStrLn rounds
  let id = parseInt $ drop (length "Game ") ids
  rounds <- sequence $ parseRound <$> split rounds "; "
  return $ Game id rounds

readInput :: String -> Maybe [Game]
readInput s = sequence $ parseGame <$> lines s

collectRoll :: Roll -> (Int, Int, Int)
collectRoll (Roll n Red) = (n, 0, 0)
collectRoll (Roll n Green) = (0, n, 0)
collectRoll (Roll n Blue) = (0, 0, n)

valid :: (Int, Int, Int) -> Bool
valid (r, g, b) = r <= 12 && g <= 13 && b <= 14

aggregateRolls :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
aggregateRolls (r1, g1, b1) (r2, g2, b2) = (max r1 r2, max g1 g2, max b1 b2)

collectColors :: Round -> (Int, Int, Int)
collectColors (Round rolls) = foldl aggregateRolls (0, 0, 0) . fmap collectRoll $ rolls


part1 :: String -> Int
part1 s = case readInput s of
  Nothing -> error "Invalid input."
  Just games -> sum $ fmap processGame games

  where
    processGame :: Game -> Int
    processGame (Game id rounds) = 
      let colorSpreads = fmap collectColors rounds
        in if all valid colorSpreads
          then id
          else 0

part2 :: String -> Int
part2 s = case readInput s of
  Nothing -> error "Invalid input."
  Just games -> sum $ fmap processGame games

  where
    processGame :: Game -> Int
    processGame (Game _ rounds) =
      let colorSpreads = fmap collectColors rounds
        in power $ foldl aggregateRolls (1, 1, 1) colorSpreads
    
    power :: (Int, Int, Int) -> Int
    power (a, b, c) = a * b * c


test_input :: String
test_input = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\n\
\Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\n\
\Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\n\
\Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\n\
\Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green\n"


getInput :: String -> Bool -> IO String
getInput filename debug = do
  if debug
    then return test_input
    else readFile filename

main :: IO ()
main = do
  let filename = "input.txt"
  let debug = False
  input <- getInput filename debug
  putStrLn $ "2023 Day 2, Part 1: " ++ show (part1 input)
  putStrLn $ "2023 Day 2, Part 2: " ++ show (part2 input)
