{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module AOC2024.Day5 (Page, part1, part2, parse, day, parser, specMap, partitionSort) where

import Common.Day qualified as D
import Common.Utils (combinations, fork, parseWith, strip, (|>))
import Control.Arrow (second)
import Control.Monad (liftM2)
import Data.Functor (($>), (<&>))
import Data.HashMap.Lazy qualified as M
import Data.HashSet qualified as HS
import Data.List (partition)
import Text.Parsec hiding (parse)
import Text.Parsec.String (Parser)

day :: D.Day Input Int
day =
  D.Day
    { D.day = "5",
      D.year = "2024",
      D.parse = parse,
      D.part1 = part1,
      D.part2 = part2
    }

type Page = Int

type PageSpec = (Page, Page)

type PageUpdate = [Page]

type Input = (HS.HashSet PageSpec, [PageUpdate])

parser :: Parser Input
parser = do
  specs <- pageSpecs
  lineBreak
  updates <- pageUpdates
  eof
  return (HS.fromList specs, updates)
  where
    lineBreak = newline >> newline $> ()

    pageSpec :: Parser PageSpec
    pageSpec = do
      x <- many1 digit
      char '|'
      y <- many1 digit
      return (read @Int x, read @Int y)

    pageSpecs :: Parser [PageSpec]
    pageSpecs = do
      x <- pageSpec
      xs <- rest
      return (x : xs)
      where
        rest =
          try (lookAhead lineBreak)
            $> []
            <|> liftM2 (:) (newline *> pageSpec) rest

    pageUpdate :: Parser PageUpdate
    pageUpdate = fmap (read @Int) <$> (many1 digit `sepBy` char ',')

    pageUpdates :: Parser [PageUpdate]
    pageUpdates = do
      x <- pageUpdate
      xs <- rest
      return (x : xs)
      where
        rest =
          try (lookAhead eof)
            $> []
            <|> liftM2 (:) (newline *> pageUpdate) rest

parse :: String -> Input
parse = parseWith parser "Day5" . strip

isUpdateInOrder :: HS.HashSet PageSpec -> PageUpdate -> Bool
isUpdateInOrder spec = all (\[x, y] -> (x, y) `elem` spec) . combinations 2

middleEntry :: PageUpdate -> Page
middleEntry = fork id (!!) ((`div` 2) . length)

part1 :: Input -> Int
part1 (spec, updates) =
  updates
    |> filter (isUpdateInOrder spec)
    |> fmap middleEntry
    |> sum

partitionSort :: M.HashMap Page (HS.HashSet Page) -> [Page] -> [Page]
partitionSort _ [] = []
partitionSort _ [x] = [x]
partitionSort m (x : xs) =
  case m M.!? x of
    Just edges ->
      let (ys, zs) = partition (`elem` edges) xs
       in partitionSort m zs ++ [x] ++ partitionSort m ys
    Nothing -> partitionSort m xs ++ [x]

specMap :: HS.HashSet PageSpec -> M.HashMap Page (HS.HashSet Page)
specMap = M.fromListWith HS.union . fmap (second HS.singleton) . HS.toList

part2 :: Input -> Int
part2 (spec, updates) =
  let invalidUpdates = filter (not . isUpdateInOrder spec) updates
      specMap' = specMap spec
   in (invalidUpdates <&> middleEntry . partitionSort specMap') |> sum