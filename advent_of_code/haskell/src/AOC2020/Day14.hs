module AOC2020.Day14 (parser, parse, part1, part2, day, zeroMask, oneMask, stringToMask) where

import Common.Day qualified as D
import Common.Utils (parseWith, strip)
import Control.Arrow (Arrow ((***)))
import Control.Monad (liftM2)
import Data.Bits (Bits (bit, complement, (.&.)), (.|.))
import Data.Functor (($>), (<&>))
import Data.HashMap.Lazy (HashMap)
import Data.HashMap.Lazy qualified as H
import Data.List (partition)
import Data.Tuple.Extra (both)
import Text.Parsec hiding (State, parse)
import Text.Parsec.String

day :: D.Day Input Int
day =
  D.Day
    { D.day = "14",
      D.year = "2020",
      D.parse = parse,
      D.part1 = part1,
      D.part2 = part2
    }

type Mask = (Int, Int)

type Addr = Int

type Assign = (Addr, Int)

type Input = [(Mask, [Assign])]

type Mem = HashMap Addr Int

baseMask :: Int
baseMask = 0b1111__1111_1111_1111_1111__1111_1111_1111_1111

zeroMask :: [Int] -> Int
zeroMask = (baseMask .&.) . complement . foldl (\a x -> a .|. bit x) 0

oneMask :: [Int] -> Int
oneMask = foldl (\a x -> a .|. bit x) 0

stringToMask :: String -> Mask
stringToMask = (zeroMask *** oneMask) . both (fmap fst) . partition ((== '0') . snd) . filter ((/= 'X') . snd) . zip [0 ..] . reverse

mask' :: Parser Mask
mask' = do
  string "mask = "
  bits <- many1 $ oneOf "01X"
  return . stringToMask $ bits

assign' :: Parser Assign
assign' = do
  string "mem["
  addr <- many1 digit <&> read @Int
  string "] = "
  value <- many1 digit <&> read @Int
  return (addr, value)

entry' :: Parser (Mask, [Assign])
entry' = do
  m <- mask' <* newline
  a <- assign' <* newline
  as <- rest
  return (m, a : as)
  where
    rest :: Parser [Assign]
    rest =
      (try . lookAhead $ (string "mask" $> ()) <|> eof) $> []
        <|> liftM2 (:) (assign' <* ((newline $> ()) <|> eof)) rest

parser :: Parser Input
parser = many1 entry' <* eof

parse :: String -> Input
parse = parseWith parser "Day14" . strip

assign :: Mem -> (Mask, [Assign]) -> Mem
assign m ((zm, om), as) = foldl go m as
  where
    go :: Mem -> Assign -> Mem
    go m' (addr, value) =
      let value' = value .&. zm .|. om
       in H.insert addr value' m'

part1 :: Input -> Int
part1 vs =
  let mem :: Mem = H.empty
      mem' = foldl assign mem vs
   in sum . H.elems $ mem'

part2 :: Input -> Int
part2 _ = 0