import Data.List (isPrefixOf, elem)
import Data.Char (ord, isSpace)
-- import Debug.Trace (trace)

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

indexOf :: String -> String -> Maybe Int
indexOf str sep = go str 0
  where
    go :: String -> Int -> Maybe Int
    go [] _ = Nothing
    go str i
      | sep `isPrefixOf` str = Just i
      | otherwise = go (tail str) (i + 1)

split :: String -> String -> [String]
split [] _ = []
split str sep = case indexOf str sep of
  Just idx -> take idx str : split (drop (idx + length sep) str) sep
  Nothing -> [str]

findIndex :: (a -> Bool) -> [a] -> Maybe Int
findIndex pred coll = go coll 0
  where
    go [] _ = Nothing
    go (x:xs) i
      | pred x = Just i
      | otherwise = go xs (i + 1)

newtype Queue a = Queue [a] deriving (Eq, Show)

enqueue :: Queue a -> a -> Queue a
enqueue (Queue q) v = Queue $ go q v
  where
    go :: [a] -> a -> [a]
    go [] v = [v]
    go (x:xs) v = x : go xs v

dequeue :: Queue a -> Maybe (a, Queue a)
dequeue (Queue []) = Nothing
dequeue (Queue (x:xs)) = Just (x, Queue xs)

findInQueue :: (a -> Bool) -> Queue a -> Maybe Int
findInQueue pred (Queue q) = findIndex pred q

type Lens = (String, Int)
type Boxes = [Queue Lens]

makeBoxes :: Boxes
makeBoxes = [Queue [] | _ <- [0..255]]

updateAt :: (a -> a) -> [a] -> Int -> [a]
updateAt f as i =
  if i < 0 || i >= length as
    then as
    else  let prefix = take i as
              v = as !! i
              suffix = drop (i + 1) as
            in prefix ++ [f v] ++ suffix

replace :: [a] -> a -> Int -> [a]
replace as a i =
  if i < 0 || i >= length as
    then as
    else  let prefix = take i as
              suffix = drop (i + 1) as
            in prefix ++ [a] ++ suffix

remove :: [a] -> Int -> [a]
remove as i =
  if i < 0 || i >= length as
    then as
    else  let prefix = take i as
              suffix = drop (i + 1) as
            in prefix ++ suffix

putLens :: Boxes -> Lens -> Boxes
putLens boxes (label, focalLength) =
  let h = hash label
    in updateAt f boxes h
  
  where
    f :: Queue Lens -> Queue Lens
    f (Queue []) = Queue [(label, focalLength)]
    f (Queue q) = case findIndex ((==) label . fst) q of
      Nothing -> enqueue (Queue q) (label, focalLength)
      Just i -> Queue $ replace q (label, focalLength) i

removeLens :: Boxes -> String -> Boxes
removeLens boxes label =
  let h = hash label
    in updateAt f boxes h

  where
    f :: Queue Lens -> Queue Lens
    f (Queue []) = Queue []
    f (Queue q) = case findIndex ((==) label . fst) q of
      Nothing -> Queue q
      Just i -> Queue $ remove q i

testInput :: String
testInput = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"

parseInt :: String -> Int
parseInt s = read s :: Int

hash :: String -> Int
hash s = foldl f 0 $ fmap ord s
  where
    f a b = ((a + b) * 17) `mod` 256

data Code = Put (String, Int) | Rm String deriving Show

part1 :: String -> Int
part1 input = sum $ hash <$> split (trim input) ","

empty :: Queue a -> Bool
empty (Queue []) = True
empty _ = False

dump :: Boxes -> String
dump boxes = foldl format "" $ filter (not . empty . snd) $ zip [0..] boxes
  where
    format :: String -> (Int, Queue Lens) -> String
    format s (boxId, Queue q) = s ++ "Box " ++ show boxId ++ ": " ++ formatLenses q ++ "\n"

    formatLenses :: [(String, Int)] -> String
    formatLenses [] = ""
    formatLenses ((label, focalLength):rest) = "[" ++ label ++ " " ++ show focalLength ++ "] " ++ formatLenses rest

part2 :: String -> Int
part2 input = 
  let codes = split (trim input) ","
      boxes = makeBoxes
    in agg $ foldl f boxes codes
  
  where
    f :: Boxes -> String -> Boxes
    f boxes code =
      case parseCode code of
        Put (label, focalLength) -> putLens boxes (label, focalLength)
        Rm label -> removeLens boxes label

    parseCode :: String -> Code
    parseCode s =
      if '=' `elem` s
        then let [code, i] = split s "="; n = parseInt i in Put (code, n)
        else Rm $ takeWhile (/='-') s

    agg :: Boxes -> Int
    agg boxes = sum $ aggQueue <$> zip [1..] boxes

    aggQueue :: (Int, Queue Lens) -> Int
    aggQueue (r, Queue q) = sum $ (\(c, (_, focalLength)) -> r * c * focalLength) <$> zip [1..] q


main :: IO ()
main = do
  let filename = "input.txt"
  let debug = False
  input <- if debug
    then return testInput
    else readFile filename
  putStrLn $ "2023 Day 15, Part 1: " ++ show (part1 input)
  putStrLn $ "2023 Day 15, Part 2: " ++ show (part2 input)
