coerce :: Char -> Int
coerce '(' = 1
coerce ')' = -1
coerce _ = 0

part1 :: String -> Int
part1 = sum . fmap coerce

part2 :: String -> Int
part2 = length . takeWhile (>=0) . scanl (+) 0 . fmap coerce

main :: IO ()
main = do
  input <- readFile "input.txt"
  putStrLn $ "2015 Day 1, Part 1: " ++ show (part1 input)
  putStrLn $ "2015 Day 1, Part 2: " ++ show (part2 input)
