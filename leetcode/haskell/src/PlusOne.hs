module PlusOne where

import Data.List (intercalate)

plusOne :: [Int] -> [Int]
plusOne = reverse . f 1 . reverse
  where
    f :: Int -> [Int] -> [Int]
    f 0 xs = xs
    f 1 (9:xs) = 0 : f 1 xs
    f 1 [] = [1]
    f 1 (x:xs) = x + 1 : xs
