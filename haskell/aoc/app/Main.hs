module Main (main) where

import AOC2024.Day14 qualified as D
import Common.Day (Day (..))
import Data.ByteString.Char8 qualified as BS
import Data.Functor ((<&>))
import Scripts.FetchInput (saveInput)
import System.Environment (getEnv)

runDay :: (Show output) => Day input output -> IO ()
runDay d = do
  input <-
    readFile ("./input/aoc" ++ d.year ++ "/day" ++ d.day ++ ".input.txt")
      <&> d.parse
  putStrLn $ d.year ++ ", Day " ++ d.day ++ ", Part 1: " ++ show (d.part1 input)
  putStrLn $ d.year ++ ", Day " ++ d.day ++ ", Part 2: " ++ show (d.part2 input)

main :: IO ()
main = do
  let d = D.day
  sessionId <- getEnv "SESSION_ID"
  putStrLn ("\n\n\n" :: String)
  saveInput (BS.pack sessionId) (BS.pack d.year) (BS.pack d.day)
  runDay d
  putStrLn ("\n\n\n" :: String)