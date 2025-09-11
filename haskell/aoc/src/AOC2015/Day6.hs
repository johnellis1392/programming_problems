{-# LANGUAGE FlexibleContexts #-}

module AOC2015.Day6 (part1, part2, parse, day, Command (..)) where

import Common.Day (Day (Day))
import Common.Day qualified as D
import Control.Monad (forM_)
import Data.Array.IO
import Data.Text qualified as T

day :: Day [Command] (IO Int)
day =
  Day
    { D.day = "6",
      D.year = "2015",
      D.parse = parse,
      D.part1 = part1,
      D.part2 = part2
    }

data Command
  = TurnOn (Int, Int) (Int, Int)
  | TurnOff (Int, Int) (Int, Int)
  | Toggle (Int, Int) (Int, Int)
  deriving (Eq, Show)

type Point = (Int, Int)

bound :: Int
bound = 1000

points :: Point -> Point -> [Point]
points (x1, y1) (x2, y2) = [(x, y) | x <- [x1 .. x2], y <- [y1 .. y2]]

runCommand ::
  (MArray IOUArray a IO) =>
  IOUArray (Int, Int) a ->
  (a -> a) ->
  (Int, Int) ->
  (Int, Int) ->
  IO ()
runCommand grid eval p1 p2 = do
  forM_ (points p1 p2) $ \(x, y) -> do
    v <- readArray grid (x, y)
    writeArray grid (x, y) $ eval v

parse :: String -> [Command]
parse = fmap (parseLine . T.words) . T.lines . T.strip . T.pack
  where
    parseLine :: [T.Text] -> Command
    parseLine ["turn", "on", p1, "through", p2] = TurnOn (parsePoint p1) (parsePoint p2)
    parseLine ["turn", "off", p1, "through", p2] = TurnOff (parsePoint p1) (parsePoint p2)
    parseLine ["toggle", p1, "through", p2] = Toggle (parsePoint p1) (parsePoint p2)
    parseLine s = error $ "illegal instruction: " ++ show s

    parsePoint :: T.Text -> (Int, Int)
    parsePoint s =
      let (x, y) = T.break (== ',') s
       in (parseInt x, parseInt (T.tail y))

    parseInt :: T.Text -> Int
    parseInt = read . T.unpack

part1 :: [Command] -> IO Int
part1 commands = do
  grid <- newArray ((0, 0), (bound - 1, bound - 1)) False
  mapM_ (eval grid) commands
  vs <- mapM (readArray grid) (points (0, 0) (bound - 1, bound - 1))
  return $ count id vs
  where
    eval grid (TurnOn p1 p2) = runCommand grid (const True) p1 p2
    eval grid (TurnOff p1 p2) = runCommand grid (const False) p1 p2
    eval grid (Toggle p1 p2) = runCommand grid not p1 p2

    count :: (a -> Bool) -> [a] -> Int
    count _ [] = 0
    count fn (a : as)
      | fn a = 1 + count fn as
      | otherwise = count fn as

part2 :: [Command] -> IO Int
part2 commands = do
  grid <- newArray ((0, 0), (bound - 1, bound - 1)) 0
  mapM_ (eval grid) commands
  vs <- mapM (readArray grid) (points (0, 0) (bound - 1, bound - 1))
  return . sum $ vs
  where
    eval grid (TurnOn p1 p2) = runCommand grid (+ 1) p1 p2
    eval grid (TurnOff p1 p2) = runCommand grid (\i -> max (i - 1) 0) p1 p2
    eval grid (Toggle p1 p2) = runCommand grid (+ 2) p1 p2