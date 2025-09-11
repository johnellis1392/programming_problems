package common

import scala.annotation.targetName
import scala.math.abs

case class Point2D(x: Int, y: Int):
  def r = y
  def c = x
  
  def north = Point2D(x, y - 1)
  def south = Point2D(x, y + 1)
  def east = Point2D(x + 1, y)
  def west = Point2D(x - 1, y)
  def move(d: Direction) = d match
    case North => north
    case South => south
    case East => east
    case West => west
  
  def +(other: Point2D): Point2D = Point2D(this.x + other.x, this.y + other.y)
  def +(other: (Int, Int)): Point2D = Point2D(this.x + other._1, this.y + other._2)

  def -(other: Point2D): Point2D = Point2D(this.x - other.x, this.y - other.y)
  def -(other: (Int, Int)): Point2D = Point2D(this.x - other._1, this.y - other._2)
  
  def manhattanDistanceTo(other: Point2D): Int = abs(this.x - other.x) + abs(this.y + other.y)
  
object Point2D:
  def fromRc(r: Int, c: Int): Point2D = Point2D(c, r)
  def origin: Point2D = Point2D(0, 0)