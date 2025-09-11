{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Common.Utils where

import Control.Arrow ((&&&))
import Data.Char (isDigit, isSpace)
import Data.HashMap.Lazy qualified as M
import Data.Hashable (Hashable)
import Data.List (dropWhileEnd, isPrefixOf, isSuffixOf, stripPrefix)
import Data.Maybe (fromJust)
import Text.Parsec qualified
import Text.Parsec.String qualified

isInt :: String -> Bool
isInt = all isDigit

parseInt :: String -> Int
parseInt = read @Int

tryParseInt :: String -> Maybe Int
tryParseInt s = if isInt s then Just (parseInt s) else Nothing

orElse :: Maybe a -> a -> a
orElse (Just a) _ = a
orElse Nothing a = a

inRange :: (Int, Int) -> Int -> Bool
inRange (start, end) n = start <= n && n <= end

(>.>) :: (a -> b) -> (b -> c) -> a -> c
(f >.> g) x = g (f x)

(>..>) :: (Functor f) => (a -> f b) -> (b -> c) -> (a -> f c)
fa >..> fb = fmap fb . fa

infixr 4 >.>

(|>) :: a -> (a -> b) -> b
x |> f = f x

infixl 1 |>

(<*<) :: (Applicative f) => f (b -> c) -> f (a -> b) -> f (a -> c)
(<*<) a b = ((.) <$> a) <*> b

infixr 4 <*<

(>*>) :: (Applicative f) => f (a -> b) -> f (b -> c) -> f (a -> c)
(>*>) = flip (<*<)

infixr 4 >*>

(<$<) :: (Functor f) => (b -> c) -> f (a -> b) -> f (a -> c)
(<$<) a b = (a .) <$> b

infixr 4 <$<

(>$>) :: (Functor f) => f (a -> b) -> (b -> c) -> f (a -> c)
(>$>) = flip (<$<)

infixr 4 >$>

combinations :: Int -> [a] -> [[a]]
combinations _ [] = []
combinations 0 _ = []
combinations 1 xs = fmap (: []) xs
combinations n (x : xs) = fmap (x :) (combinations (n - 1) xs) ++ combinations n xs

-- replicateM 2 [1,2,3] -- Generate 2-combinations of list

count :: (a -> Bool) -> [a] -> Int
count _ [] = 0
count p (x : xs)
  | p x = 1 + count p xs
  | otherwise = count p xs

fork :: (a -> b) -> (b -> c -> d) -> (a -> c) -> a -> d
fork f h g a = h (f a) (g a)

strip :: String -> String
strip = dropWhile isSpace . dropWhileEnd isSpace

split :: String -> String -> [String]
split _ [] = []
split p s = case findSubsequence p s of
  Just i ->
    let (a, as) = splitAt i s
     in a : split p (fromJust $ stripPrefix p as)
  Nothing -> []

findSubsequence :: (Eq a) => [a] -> [a] -> Maybe Int
findSubsequence s xs = findSubsequence' s xs 0
  where
    findSubsequence' _ [] _ = Nothing
    findSubsequence' sub xs' n
      | sub `isPrefixOf` xs' = Just n
      | otherwise = findSubsequence' sub (tail xs') (n + 1)

stripSuffix :: (Eq a) => [a] -> [a] -> Maybe [a]
stripSuffix suffix xs =
  if suffix `isSuffixOf` xs
    then Just $ take (length xs - length suffix) xs
    else Nothing

zipWithIndex :: [a] -> [(Int, a)]
zipWithIndex = zip [0 ..]

parseWith :: Text.Parsec.String.Parser a -> Text.Parsec.SourceName -> String -> a
parseWith parser source input = case Text.Parsec.parse parser source input of
  Left err -> error $ "An error occurred while parsing: " ++ show err
  Right result -> result

toSnd :: (a -> b) -> a -> (a, b)
toSnd f = id &&& f

toFst :: (b -> c) -> b -> (c, b)
toFst f = f &&& id

groupWith :: (Hashable b) => (a -> b) -> [a] -> M.HashMap b [a]
groupWith f = foldl (\m (k, v) -> M.insertWith (++) k v m) M.empty . fmap (f &&& (: []))

nwise :: Int -> [a] -> [[a]]
nwise n xs
  | length xs < n = []
  | otherwise = take n xs : nwise n (drop 1 xs)

primeFactors :: (Integral t) => t -> [t]
primeFactors n =
  case factors of
    [] -> [n]
    _ -> factors ++ primeFactors (n `div` head factors)
  where
    factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2 .. n - 1]