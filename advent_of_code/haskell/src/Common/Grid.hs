module Common.Grid
  ( Grid (..),
    makeGrid,
    get,
    contains,
    neighbors,
    adjacents,
    values,
    pointSet,
    find,
    get',
    set,
  )
where

import Common.Points (Point2i)
import Common.Points qualified as P
import Common.Utils (fork, (|>))
import Data.Array (Array, array, bounds, elems, inRange, indices, (!), (//))
import Data.Functor ((<&>))

data Grid a where
  Grid :: Array Point2i a -> Grid a
  deriving (Show, Eq)

makeGrid :: forall a. [[a]] -> Grid a
makeGrid xs =
  let rows = length xs
      cols = length (head xs)
      b :: Point2i = (rows - 1, cols - 1)
   in Grid $ array (origin, b) values'
  where
    origin :: Point2i
    origin = (0, 0)

    values' :: [(Point2i, a)]
    values' =
      zip [0 ..] xs
        >>= ( \(r, row) ->
                zip [0 ..] row <&> \(c, v) -> ((r, c) :: Point2i, v)
            )

get :: Grid a -> Point2i -> a
get (Grid g) p = g ! p

get' :: Grid a -> Point2i -> Maybe a
get' (Grid g) p = if inRange (bounds g) p then Just (g ! p) else Nothing

getAll :: Grid a -> [Point2i] -> [(Point2i, a)]
getAll g ps = filter (contains g) ps <&> (\p -> (p, get g p))

contains :: Grid a -> Point2i -> Bool
contains (Grid arr) (r, c) =
  let (rows, cols) = snd $ bounds arr
   in 0 <= r && r <= rows && 0 <= c && c <= cols

adjacents :: Grid a -> Point2i -> [(Point2i, a)]
adjacents g p = P.adjacents p |> getAll g

neighbors :: Grid a -> Point2i -> [(Point2i, a)]
neighbors g p = P.neighbors p |> getAll g

values :: Grid a -> [(Point2i, a)]
values (Grid arr) = fork indices zip elems arr

pointSet :: Grid a -> [Point2i]
pointSet (Grid arr) = indices arr

find :: forall a. Grid a -> (a -> Bool) -> Maybe (Point2i, a)
find g f = go f (values g)
  where
    go :: (a -> Bool) -> [(Point2i, a)] -> Maybe (Point2i, a)
    go _ [] = Nothing
    go f' ((p, x) : xs)
      | f' x = Just (p, x)
      | otherwise = go f' xs

set :: Grid a -> Point2i -> a -> Grid a
set (Grid arr) p v = Grid $ arr // [(p, v)]