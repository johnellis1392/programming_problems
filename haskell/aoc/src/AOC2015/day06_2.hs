#!/usr/bin/env stack
{- stack runghc --resolver lts-6.0
  --install-ghc
  --package regex-tdfa
  --package vector
-}

-- import Data.Functor ((<&>))
import Data.Char (isSpace)
import Data.List (isPrefixOf)
import Text.Regex.TDFA ((=~))
-- import qualified Data.Vector as V
-- import Data.Vector ((//), (!))
import Debug.Trace (trace)
import qualified Data.Set as Set

trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

find s v = go s v 0
  where
    go _ [] _ = Nothing
    go s v i
      | s `isPrefixOf` v = Just i
      | otherwise = go s (drop 1 v) (i + 1)

split sep str = case find sep str of
  Nothing -> [str]
  Just i -> take i str : split sep (drop (i + length sep) str)

type Point = (Int, Int)
showPoint (x, y) = "(" ++ show x ++ ", " ++ show y ++ ")"

data Command =
    TurnOn Point Point
  | TurnOff Point Point
  | Toggle Point Point


instance Show Command where
  show (TurnOn p1 p2) = "turn on (" ++ showPoint p1 ++ ") through (" ++ showPoint p2 ++ ")"
  show (TurnOff p1 p2) = "turn off (" ++ showPoint p1 ++ ") through (" ++ showPoint p2 ++ ")"
  show (Toggle p1 p2) = "toggle (" ++ showPoint p1 ++ ") through (" ++ showPoint p2 ++ ")"

parseInt s = read s :: Int

turnOnReg = "^turn on ([0-9]+),([0-9]+) through ([0-9]+),([0-9]+)$"
turnOffReg = "^turn off ([0-9]+),([0-9]+) through ([0-9]+),([0-9]+)$"
toggleReg = "^toggle ([0-9]+),([0-9]+) through ([0-9]+),([0-9]+)$"

parseInput :: String -> [Command]
parseInput = fmap (parseLine . trim) . split "\n" . trim
  where
    parseLine s
      | s =~ turnOnReg :: Bool =
        let (_, _, _, [x1, y1, x2, y2]) = s =~ turnOnReg :: (String, String, String, [String])
        in TurnOn (parseInt x1, parseInt y1) (parseInt x2, parseInt y2)
      | s =~ turnOffReg :: Bool =
        let (_, _, _, [x1, y1, x2, y2]) = s =~ turnOffReg :: (String, String, String, [String])
        in TurnOff (parseInt x1, parseInt y1) (parseInt x2, parseInt y2)
      | s =~ toggleReg :: Bool =
        let (_, _, _, [x1, y1, x2, y2]) = s =~ toggleReg :: (String, String, String, [String])
        in Toggle (parseInt x1, parseInt y1) (parseInt x2, parseInt y2)
      | otherwise = error $ "Unmatched pattern: " ++ s

-- type Grid = V.Vector (V.Vector Bool)
-- width = 1000
-- height = 1000
-- makeGrid :: Grid
-- makeGrid = V.generate height (const $ V.generate width (const False))

-- get :: Grid -> Point -> Bool
-- get grid (x, y) = (grid ! y) ! x

-- set :: Grid -> Point -> Bool -> Grid
-- set grid (x, y) v = let
--     row = grid ! y
--     row' = row // [(x, v)]
--   in grid // [(y, row')]

-- toggle :: Grid -> Point -> Grid
-- toggle g (x, y) = set g (x, y) $ not $ get g (x, y)

-- turnOn :: Grid -> Point -> Grid
-- turnOn grid (x, y) = set grid (x, y) True

-- turnOff :: Grid -> Point -> Grid
-- turnOff grid (y, x) = set grid (x, y) False


-- eval :: Grid -> Command -> Grid
-- eval grid (TurnOn (x1, y1) (x2, y2)) =
--   let points = [(x, y) | x <- [x1..x2], y <- [y1..y2]]
--   in foldl turnOn grid points
-- eval grid (TurnOff (x1, y1) (x2, y2)) =
--   let points = [(x, y) | x <- [x1..x2], y <- [y1..y2]]
--   in foldl turnOff grid points
-- eval grid (Toggle (x1, y1) (x2, y2)) =
--   let points = [(x, y) | x <- [x1..x2], y <- [y1..y2]]
--   in foldl toggle grid points

-- part1 commands = 
--   let grid = makeGrid
--   in sum $ sum . fmap toInt <$> foldl eval' grid commands
--   where
--     toInt True = 1
--     toInt False = 0
--     eval' g c = trace ("Running command: " ++ show c) (eval g c)

eval :: Set.Set Point -> Command -> Set.Set Point
eval grid (TurnOn (x1, y1) (x2, y2)) =
  let points = [(x, y) | x <- [x1..x2], y <- [y1..y2]]
  in foldl (flip Set.insert) grid points
eval grid (TurnOff (x1, y1) (x2, y2)) =
  let points = [(x, y) | x <- [x1..x2], y <- [y1..y2]]
  in foldl (flip Set.delete) grid points
eval grid (Toggle (x1, y1) (x2, y2)) =
  let points = [(x, y) | x <- [x1..x2], y <- [y1..y2]]
  in foldl toggle grid points
  where
    toggle s p = if p `elem` s then Set.delete p s else Set.insert p s

part1 commands =
  let s = Set.empty :: Set.Set Point
  in foldr (flip eval) s commands

part2 _ = 0

main :: IO ()
main = do
  input <- readFile "input.txt" >>= return . parseInput
  -- input <- readFile "input.txt" <&> parseInput
  -- mapM_ print input
  putStrLn $ "2015 Day 6, Part 1: " ++ show (part1 input)
  putStrLn $ "2015 Day 6, Part 2: " ++ show (part2 input)
