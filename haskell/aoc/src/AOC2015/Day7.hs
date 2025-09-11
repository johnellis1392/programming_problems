module AOC2015.Day7 (part1, part2, parse, day) where

import Common.Day (Day (Day))
import Common.Day qualified as D
import Data.Bits hiding (And)
import Data.Char (isDigit)
import Data.Map ((!))
import Data.Map qualified as M

day :: Day CircuitMap Int
day =
  Day
    { D.day = "7",
      D.year = "2015",
      D.parse = parse,
      D.part1 = part1,
      D.part2 = part2
    }

data Target
  = Const Int
  | Ref String
  deriving (Show, Eq)

data Circuit
  = Send Target String
  | And Target Target String
  | Or Target Target String
  | LShift Target Target String
  | RShift Target Target String
  | Not Target String
  deriving (Eq, Show)

type Cache = M.Map String Int

type CircuitMap = M.Map String Circuit

parse :: String -> M.Map String Circuit
parse =
  foldl (\m (s, circuit) -> M.insert s circuit m) M.empty
    . fmap (parse' . words)
    . lines
  where
    isInt :: String -> Bool
    isInt = all isDigit

    parseInt :: String -> Int
    parseInt = read

    parseTarget :: String -> Target
    parseTarget s
      | isInt s = Const $ parseInt s
      | otherwise = Ref s

    parse' :: [String] -> (String, Circuit)
    parse' [i, "->", s] = (s, Send (parseTarget i) s)
    parse' [a, "AND", b, "->", s] = (s, And (parseTarget a) (parseTarget b) s)
    parse' [a, "OR", b, "->", s] = (s, Or (parseTarget a) (parseTarget b) s)
    parse' [a, "LSHIFT", b, "->", s] = (s, LShift (parseTarget a) (parseTarget b) s)
    parse' [a, "RSHIFT", b, "->", s] = (s, RShift (parseTarget a) (parseTarget b) s)
    parse' ["NOT", v, "->", s] = (s, Not (parseTarget v) s)
    parse' v = error $ "Illegal circuit configuration: " ++ show v

sink :: Circuit -> String
sink (Send _ s) = s
sink (And _ _ s) = s
sink (Or _ _ s) = s
sink (LShift _ _ s) = s
sink (RShift _ _ s) = s
sink (Not _ s) = s

evalTarget :: Target -> CircuitMap -> Cache -> (Int, Cache)
evalTarget (Const n) _ cache = (n, cache)
evalTarget (Ref s) circuitMap cache = eval (circuitMap ! s) circuitMap cache

eval :: Circuit -> CircuitMap -> Cache -> (Int, Cache)
eval circuit circuitMap cache =
  if sink circuit `M.member` cache
    then (cache ! sink circuit, cache)
    else case circuit of
      Send t _ ->
        let (res, cache') = evalTarget t circuitMap cache
         in (res, M.insert (sink circuit) res cache')
      And l r _ ->
        let (l', cache') = evalTarget l circuitMap cache
            (r', cache'') = evalTarget r circuitMap cache'
            res = l' .&. r'
         in (res, M.insert (sink circuit) res cache'')
      Or l r _ ->
        let (l', cache') = evalTarget l circuitMap cache
            (r', cache'') = evalTarget r circuitMap cache'
            res = l' .|. r'
         in (res, M.insert (sink circuit) res cache'')
      LShift l r _ ->
        let (l', cache') = evalTarget l circuitMap cache
            (r', cache'') = evalTarget r circuitMap cache'
            res = l' `shiftL` r'
         in (res, M.insert (sink circuit) res cache'')
      RShift l r _ ->
        let (l', cache') = evalTarget l circuitMap cache
            (r', cache'') = evalTarget r circuitMap cache'
            res = l' `shiftR` r'
         in (res, M.insert (sink circuit) res cache'')
      Not t _ ->
        let (t', cache'') = evalTarget t circuitMap cache
            res = complement t'
         in (res, M.insert (sink circuit) res cache'')

part1 :: CircuitMap -> Int
part1 circuits =
  let origin = circuits ! "a"
      (res, _) = eval origin circuits M.empty
   in res

part2 :: CircuitMap -> Int
part2 circuits =
  let v = part1 circuits
      circuits' = M.insert "b" (Send (Const v) "b") circuits
      origin = circuits' ! "a"
      (res, _) = eval origin circuits' M.empty
   in res