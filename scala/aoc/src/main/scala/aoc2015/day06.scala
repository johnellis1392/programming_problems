package aoc2015

import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap

sealed trait CommandType
case object TurnOn extends CommandType
case object TurnOff extends CommandType
case object Toggle extends CommandType
case class Point(x: Int, y: Int)
case class Command(t: CommandType, from: Point, to: Point)

def part1(commands: Array[Command]): Long =
  val set = HashSet[Point]()
  for 
    cmd <- commands
    y <- cmd.from.y to cmd.to.y
    x <- cmd.from.x to cmd.to.x
  do
    val p = Point(x, y)
    cmd.t match
      case TurnOn => set += p
      case TurnOff => set -= p
      case Toggle => if set.contains(p) then set -= p else set += p
  set.size

def part2(commands: Array[Command]): Long =
  val vs = HashMap[Point, Long]()
  for
    cmd <- commands
    y <- cmd.from.y to cmd.to.y
    x <- cmd.from.x to cmd.to.x
  do
    val p = Point(x, y)
    cmd.t match
      case TurnOn if vs.contains(p) => vs(p) += 1L
      case TurnOn => vs(p) = 1L
      case TurnOff if vs.contains(p) => if vs(p) <= 1 then vs -= p else vs(p) -= 1L
      case TurnOff => // Skip
      case Toggle if vs.contains(p) => vs(p) += 2L
      case Toggle => vs(p) = 2L
  vs.values.sum

@main def main =
  val turnon_re = "^turn on (\\d+),(\\d+) through (\\d+),(\\d+)$".r
  val turnoff_re = "^turn off (\\d+),(\\d+) through (\\d+),(\\d+)$".r
  val toggle_re = "^toggle (\\d+),(\\d+) through (\\d+),(\\d+)$".r
  val commands = io.Source.fromFile("input.txt").getLines()
    .map(_.trim()).filter(_.nonEmpty).map:
      case turnon_re(x1, y1, x2, y2) => Command(TurnOn, Point(x1.toInt, y1.toInt), Point(x2.toInt, y2.toInt))
      case turnoff_re(x1, y1, x2, y2) => Command(TurnOff, Point(x1.toInt, y1.toInt), Point(x2.toInt, y2.toInt))
      case toggle_re(x1, y1, x2, y2) => Command(Toggle, Point(x1.toInt, y1.toInt), Point(x2.toInt, y2.toInt))
    .toArray
  
  println(s"2015 Day 6, Part 1: ${part1(commands)}")
  println(s"2015 Day 6, Part 2: ${part2(commands)}")
