import Data.Char (isSpace)
import Data.Functor ((<&>))
import Data.List (isPrefixOf, minimum, sort)
import Debug.Trace (trace)

trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

find x y = go x y 0
  where
    go _ [] _ = Nothing
    go x ys i
      | x `isPrefixOf` ys = Just i
      | otherwise = go x (tail ys) (i+1)


split sep v = case find sep v of
  Nothing -> return v
  Just i -> take i v : split sep (drop (i + length sep) v)

parseInt s = read s :: Int

type Gift = (Int, Int, Int)

parseInput :: String -> [Gift]
parseInput s = (filter (not . null) . split "\n" . trim) s 
  <&> split "x" 
  <&> fmap parseInt
  <&> \[l, w, h] -> (l, w, h)

part1 :: String -> Int
part1 s = sum $ parseInput s <&> f
  where
    f :: Gift -> Int
    f (l, w, h) = let 
        sides = [l * w, l * h, w * h]
        ribbonLength = minimum sides
      in (sum . fmap (*2) $ sides) + ribbonLength


part2 :: String -> Int
part2 s = sum $ parseInput s <&> f
  where
    f :: Gift -> Int
    f (l, w, h) = let
        sides = take 2 . sort $ [l, w, h]
        volume = l * w * h
      in sum (sides <&> (*2)) + volume

testInput = "\n\
\2x3x4\n\
\1x1x10\n\
\\n"

main :: IO ()
main = do
  let debug = False
  input <- (if debug
    then return testInput
    else readFile "input.txt") <&> trim
  putStrLn $ "2015 Day 2, Part 1: " ++ show (part1 input)
  putStrLn $ "2015 Day 2, Part 2: " ++ show (part2 input)
