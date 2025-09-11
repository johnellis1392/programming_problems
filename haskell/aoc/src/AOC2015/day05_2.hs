import Data.Set (empty, insert, member, Set)
import Data.Char (isSpace)
import Data.List (isPrefixOf)
import Data.Functor ((<&>))
import Debug.Trace (trace)

-- testInput = "\
-- \ugknbfddgicrmopn\n\
-- \aaa\n\
-- \jchzalrnumimnmhp\n\
-- \haegwjzuvuyypxyu\n\
-- \dvszwmarrgswjxmb\n\
-- \\n"

testInput = "\
\qjhvhtzxzqqjkmpb\n\
\xxyxx\n\
\uurcxstgmygtbstg\n\
\ieodomkazucvgmuy\n\
\\n"

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

find s v = go s v 0
  where
    go _ [] _ = Nothing
    go s vs i
      | s `isPrefixOf` vs = Just i
      | otherwise = go s (tail vs) (i + 1)

split :: String -> String -> [String]
split sep str = case find sep str of
  Nothing -> [str]
  Just i -> take i str : split sep (drop (i + length sep) str)

parseInput :: String -> [String]
parseInput = fmap trim . split "\n" . trim

both f (a, b) = (f a, f b)

isVowel c = c `elem` ['a', 'e', 'i', 'o', 'u']

count :: (a -> Bool) -> [a] -> Int
count pred as = go as 0
  where go [] n = n
        go (a:as) n = if pred a then go as (n + 1) else go as n

part1 :: [String] -> Int
part1 = count passes
  where
    passes s = passes' s (0, 0)
    passes' ('a':'b':_) _ = False
    passes' ('c':'d':_) _ = False
    passes' ('p':'q':_) _ = False
    passes' ('x':'y':_) _ = False
    passes' (a:b:rest) (x, y) =
      let vowelCount = if isVowel a then x + 1 else x
          doubleCount = if a == b then y + 1 else y
        in passes' (b:rest) (vowelCount, doubleCount)
    passes' [a] (x, y) = if isVowel a then passes' [] (x + 1, y) else passes' [] (x, y)
    passes' [] (x, y) = x >= 3 && y >= 1
    
    -- I like this cuz it's clever, but it doesn't technically work...
    -- passes' [] (x, y) = uncurry (&&) . both (>0) $ (x, y)


part2 :: [String] -> Int
part2 = length . filter check2 . filter check1
  where
    check1 (a:b:c:rest)
      | a == c = True
      | otherwise = check1 (b:c:rest)
    check1 _ = False

    check2 (a:b:rest) = case find [a, b] rest of
      Nothing -> check2 (b:rest)
      Just _ -> True
    check2 _ = False


main :: IO ()
main = do
  let debug = False
  input <- (
    if debug
      then return testInput
      else readFile "input.txt"
    ) <&> parseInput
  putStrLn $ "2015 Day 5, Part 1: " ++ show (part1 input)
  putStrLn $ "2015 Day 5, Part 2: " ++ show (part2 input)
  
