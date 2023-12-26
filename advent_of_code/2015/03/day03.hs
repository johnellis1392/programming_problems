import Data.Set (fromList, union)
import Data.Functor ((<&>))
import Data.List (partition)

type Point = (Int, Int)

(<+>) :: Point -> Point -> Point
(<+>) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

direction :: Char -> Point
direction '^' = (0, 1)
direction 'v' = (0, -1)
direction '>' = (1, 0)
direction '<' = (-1, 0)
direction _= (0, 0)

part1 :: String -> Int
part1 = length . fromList . scanl (<+>) (0, 0) . fmap direction

zipWithIndex :: [a] -> [(Int, a)]
zipWithIndex = zip [0..]

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

f = let (santa, robo) = both (fmap snd) . partition (even . fst) $ zipWithIndex "^>v<"
  in fromList . scanl (<+>) (0, 0) . fmap direction $ santa

part2 :: String -> Int
part2 s = let (santa, robo) = both (fmap snd) . partition (even . fst) $ zipWithIndex s
  in length $ g santa `union` g robo
  where
    g = fromList . scanl (<+>) (0, 0) . fmap direction

main :: IO ()
main = do
  input <- readFile "input.txt"
  putStrLn $ "2015 Day 3, Part 1: " ++ show (part1 input)
  putStrLn $ "2015 Day 3, Part 2: " ++ show (part2 input)
