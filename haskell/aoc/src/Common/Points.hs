{-# LANGUAGE DeriveAnyClass #-}

module Common.Points
  ( Point2i,
    Point2f,
    Point3i,
    Point3f,
    Vertex2i,
    Direction (..),
    adjacents,
    diagonals,
    move,
    neighbors,
    turnrv,
    turnlv,
    turnl,
    turnr,
    moverc,
  )
where

import Common.Utils (fork)
import Data.Functor ((<&>))
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

data Direction = North | South | East | West
  deriving (Eq, Show, Generic, Hashable)

type Point2i = (Int, Int)

type Point2f = (Double, Double)

type Point3i = (Int, Int, Int)

type Point3f = (Double, Double, Double)

type Vertex2i = (Point2i, Direction)

adjacents :: Point2i -> [Point2i]
adjacents p = [North, South, East, West] <&> move p

diagonals :: Point2i -> [Point2i]
diagonals p = ds <&> foldl move p
  where
    ds :: [[Direction]]
    ds = [[North, East], [North, West], [South, East], [South, West]]

neighbors :: Point2i -> [Point2i]
neighbors = fork adjacents (++) diagonals

move :: Point2i -> Direction -> Point2i
move (x, y) North = (x, y + 1)
move (x, y) South = (x, y - 1)
move (x, y) East = (x + 1, y)
move (x, y) West = (x - 1, y)

moverc :: Point2i -> Direction -> Point2i
moverc (r, c) North = (r - 1, c)
moverc (r, c) South = (r + 1, c)
moverc (r, c) East = (r, c + 1)
moverc (r, c) West = (r, c - 1)

turnl :: Direction -> Direction
turnl North = West
turnl West = South
turnl South = East
turnl East = North

turnr :: Direction -> Direction
turnr North = East
turnr East = South
turnr South = West
turnr West = North

turnlv :: Vertex2i -> Vertex2i
turnlv (p, d) = (p, turnl d)

turnrv :: Vertex2i -> Vertex2i
turnrv (p, d) = (p, turnr d)