module AOC2020.Day13 (parser, parse, part1, part2, day, inv, crt, translate) where

import Common.Day qualified as D
import Common.Utils (parseWith, strip, (|>))
import Control.Arrow (first, second)
import Control.Monad (zipWithM)
import Data.Functor (($>), (<&>))
import Data.List (find)
import Data.List.Extra (minimumOn)
import Data.Maybe (fromJust)
import Text.Parsec hiding (State, parse)
import Text.Parsec.String

day :: D.Day Input Int
day =
  D.Day
    { D.day = "13",
      D.year = "2020",
      D.parse = parse,
      D.part1 = part1,
      D.part2 = part2
    }

type Timestamp = Int

data Bus
  = Active Timestamp
  | Inactive

type Input = (Timestamp, [Bus])

parser :: Parser Input
parser = do
  timestamp <- read @Int <$> many1 digit
  newline
  buses <- bus `sepBy` char ','
  eof
  return (timestamp, buses)
  where
    bus :: Parser Bus
    bus =
      choice
        [ char 'x' $> Inactive,
          Active <$> (many1 digit <&> read @Int)
        ]
        <?> "([0-9]+,x)"

parse :: String -> Input
parse = parseWith parser "Day13" . strip

isActive :: Bus -> Bool
isActive (Active _) = True
isActive _ = False

fromActive :: Bus -> Timestamp
fromActive (Active x) = x
fromActive _ = error "Cannot extract value from Inactive Bus"

part1 :: Input -> Int
part1 (t, buses) =
  filter isActive buses
    <&> (\(Active t') -> (t', t' - (t `mod` t')))
      |> minimumOn snd
      |> uncurry (*)

translate :: [Bus] -> [(Int, Timestamp)]
translate = fmap (second fromActive) . filter (isActive . snd) . zip [0 ..]

inv :: Int -> Int -> Maybe Int
inv a m = find (\x -> ((a `mod` m) * (x `mod` m)) `mod` m == 1) [0 .. m - 1]

-- Chinese Remainder Theorom:
-- [(Remainder, Dividend)]
crt :: [(Int, Int)] -> Int
crt xs =
  let (rems, nums) = unzip xs
      prod = product nums
      pp = fmap (prod `div`) nums
      invs = fromJust $ zipWithM inv pp nums
      xs' = (\(a, b, c) -> a * b * c) <$> zip3 rems pp invs
   in sum xs' `mod` prod

part2 :: Input -> Int
part2 (_, buses) =
  let buses' = translate buses
      n = fst . last $ buses'
      vs = fmap (first (n -)) buses'
   in crt vs - n