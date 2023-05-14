import Data.List (intercalate)

plusOne :: [Int] -> [Int]
plusOne = reverse . f 1 . reverse
  where
    f :: Int -> [Int] -> [Int]
    f 0 xs = xs
    f 1 (9:xs) = 0 : f 1 xs
    f 1 [] = [1]
    f 1 (x:xs) = x + 1 : xs

tests :: [([Int], [Int])]
tests = [
  ([1,2,3], [1,2,4]),
  ([4,3,2,1], [4,3,2,2]),
  ([9], [1,0])]

runTest :: ([Int], [Int]) -> String
runTest (iv, ov) =
  let r = plusOne iv
  in if r == ov
     then "Success"
     else "Failure"

main :: IO ()
main = do
  putStrLn "Running..."
  putStrLn $ intercalate "\n" $ runTest <$> tests
