module AOC2015.Day4 (part1, part2, parse, day) where

import Common.Day (Day (Day))
import Common.Day qualified as D
import Common.Utils (strip)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Digest.Pure.MD5 (md5)
import Data.List (isPrefixOf)

day :: Day String Int
day =
  Day
    { D.day = "4",
      D.year = "2015",
      D.parse = parse,
      D.part1 = part1,
      D.part2 = part2
    }

parse :: String -> String
parse = strip

go :: String -> String -> Int -> Int
go hashPrefix searchPrefix i =
  let hash = show . md5 . BS.pack $ hashPrefix ++ show i
   in if searchPrefix `isPrefixOf` hash
        then i
        else go hashPrefix searchPrefix (i + 1)

part1 :: String -> Int
part1 s = go s "00000" 0

part2 :: String -> Int
part2 s = go s "000000" 0