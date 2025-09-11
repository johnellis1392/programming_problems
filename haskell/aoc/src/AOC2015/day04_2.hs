#!/usr/bin/env stack
{- stack runghc --resolver lts-6.0
  --install-ghc
  --package pureMD5
-}

-- NOTE: Run with `stack day04_2.hs`

import Data.ByteString.Lazy.Char8 (pack)
import Data.Digest.Pure.MD5 (md5)
import Data.List (isPrefixOf)

md5Hash :: String -> String
md5Hash = show . md5 . pack

part1 :: String -> Int
part1 key = go key 0
  where
    go :: String -> Int -> Int
    go k n = let hash = md5Hash (k ++ show n)
      in if "00000" `isPrefixOf` hash
        then n
        else go k (n + 1)

part2 :: String -> Int
part2 key = go key 0
  where
    go :: String -> Int -> Int
    go k n = let hash = md5Hash (k ++ show n)
      in if "000000" `isPrefixOf` hash
        then n
        else go k (n + 1)

main :: IO ()
main = do
  -- let input = pack "abcdef609043"
  -- let md5Digest = md5 input
  -- putStrLn $ "MD5 Hash: " ++ show md5Digest
  let key = "iwrupvqb"
  putStrLn $ "2015 Day 4, Part 1: " ++ show (part1 key)
  putStrLn $ "2015 Day 4, Part 2: " ++ show (part2 key)
