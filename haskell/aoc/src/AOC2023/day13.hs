import Data.Char (isSpace)
import Data.List (isPrefixOf, singleton, intercalate)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)

type Grid = [[String]]

indexOf :: Eq a => [a] -> [a] -> Maybe Int
indexOf [] _ = Nothing
indexOf a b = if b `isPrefixOf` a then Just 0 else indexOf (tail a) b <&> (+1)

split :: String -> String -> [String]
split [] _ = []
split str sep = case str `indexOf` sep of
                  Just i -> take i str : split (drop (i + length sep) str) sep
                  Nothing -> [str]

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

readInput :: String -> [Grid]
readInput s = split s "\n\n" <&> \g -> filter (not . null) (split g "\n") <&> map singleton

dumpGrids :: [Grid] -> String
dumpGrids = concatMap dumpGrid
  where
    dumpGrid :: Grid -> String
    dumpGrid grid = (intercalate "\n" . fmap concat $ grid) ++ "\n\n"

range :: Int -> Int -> [Int]
range i n
  | i >= n = []
  | otherwise = i : range (i + 1) n

count :: (a -> Bool) -> [a] -> Int
count _ [] = 0
count f (a:as) = if f a then 1 + count f as else count f as

get :: [a] -> Maybe a
get [] = Nothing
get (a:_) = Just a

mirrorRow :: Grid -> Maybe Int
mirrorRow grid = get $ filter numDifferences (range 1 (length grid))
  where
    numDifferences :: Int -> Bool
    numDifferences i = all (and . uncurry (zipWith (==))) (zip (reverse . take i $ grid) (drop i grid))

getOrElse :: a -> Maybe a -> a
getOrElse _ (Just a) = a
getOrElse a Nothing = a

columns :: Grid -> Grid
columns grid = range 0 (length . head $ grid) <&> \i -> fmap (!! i) grid


part1 :: [Grid] -> Int
part1 = sum . fmap getMirrorRows
  where
    getMirrorRows grid = case mirrorRow grid of
      Just r -> 100 * r
      Nothing -> fromMaybe 0 (mirrorRow . columns $ grid)


mirrorRowWithSmudges :: Grid -> Maybe Int
mirrorRowWithSmudges grid = get $ filter numDifferences (range 1 (length grid))
  where
    numDifferences :: Int -> Bool
    numDifferences i = 1 == sum (zip (reverse . take i $ grid) (drop i grid) <&> count id . uncurry (zipWith (/=)))

part2 :: [Grid] -> Int
part2 = sum . fmap getMirrorRows
  where
    getMirrorRows grid = case mirrorRowWithSmudges grid of
      Just r -> 100 * r
      Nothing -> fromMaybe 0 (mirrorRowWithSmudges . columns $ grid)


test_input = "\n\
\#.##..##.\n\
\..#.##.#.\n\
\##......#\n\
\##......#\n\
\..#.##.#.\n\
\..##..##.\n\
\#.#.##.#.\n\
\\n\
\#...##..#\n\
\#....#..#\n\
\..##..###\n\
\#####.##.\n\
\#####.##.\n\
\..##..###\n\
\#....#..#\n\
\\n"

main :: IO ()
main = do
  let filename = "input.txt"
  let debug = False
  input <-
    if debug then return test_input
             else readFile filename
  let grids = readInput input
  -- putStrLn $ "Grids: \n" ++ dumpGrids grids
  putStrLn $ "2023 Day 13, Part 1: " ++ show (part1 grids)
  putStrLn $ "2023 Day 13, Part 2: " ++ show (part2 grids)
