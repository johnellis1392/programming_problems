import Data.Char (isSpace, isDigit)
import Data.List (isPrefixOf)

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

readInput :: String -> [String]
readInput input = filter (not . null) $ trim <$> lines input

parseInt :: String -> Int
parseInt s = read s :: Int

singleton :: a -> [a]
singleton a = [a]

fork :: (b -> c -> d) -> (a -> b) -> (a -> c) -> a -> d
fork f g h a = f (g a) (h a)


part1 :: String -> Int
part1 s = sum $ processLine <$> readInput s
  where
    processLine :: String -> Int
    processLine s = parseInt $ fork (++) (singleton . head) (singleton . last) $ filter isDigit s


part2 :: String -> Int
part2 s = sum $ processLine <$> readInput s
  where
    processLine :: String -> Int
    processLine s = parseInt $ fork (++) (singleton . head) (singleton . last) $ parseLine s

    parseLine :: String -> String
    parseLine [] = ""
    parseLine s
      | "one"    `isPrefixOf` s = '1' : parseLine (tail s)
      | "two"    `isPrefixOf` s = '2' : parseLine (tail s)
      | "three"  `isPrefixOf` s = '3' : parseLine (tail s)
      | "four"   `isPrefixOf` s = '4' : parseLine (tail s)
      | "five"   `isPrefixOf` s = '5' : parseLine (tail s)
      | "six"    `isPrefixOf` s = '6' : parseLine (tail s)
      | "seven"  `isPrefixOf` s = '7' : parseLine (tail s)
      | "eight"  `isPrefixOf` s = '8' : parseLine (tail s)
      | "nine"   `isPrefixOf` s = '9' : parseLine (tail s)
      | isDigit (head s) = head s : parseLine (tail s)
      | otherwise = parseLine (tail s)


test_input :: String
test_input = "1abc2\n\
\pqr3stu8vwx\n\
\a1b2c3d4e5f\n\
\treb7uchet"

test_input2 :: String
test_input2 = "two1nine\n\
\eightwothree\n\
\abcone2threexyz\n\
\xtwone3four\n\
\4nineeightseven2\n\
\zoneight234\n\
\7pqrstsixteen\n"

getInput :: Bool -> IO String
getInput debug = do
  let filename = "input.txt"
  if debug
    then return test_input
    else readFile filename

main :: IO ()
main = do
  let filename = "input.txt"
  let debug = False
  input <- getInput debug
  print "Hello, World!"
  print $ part1 input
  print $ "2023 Day 1, Part 1: " ++ show (part1 input)
  print $ "2023 Day 1, Part 2: " ++ show (part2 input)
