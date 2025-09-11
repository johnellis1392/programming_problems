package aoc2015

import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet

case class Point(x: Long, y: Long):
  override def toString() = s"($x, $y)"
  def north = Point(x, y + 1)
  def south = Point(x, y - 1)
  def east = Point(x + 1, y)
  def west = Point(x - 1, y)


def part1(input: String): Long = 
  val dropoffs = HashSet[Point]()
  val source = Point(0, 0)
  dropoffs += source
  var curr = source
  input.foreach:
    c =>
      curr = c match
        case '^' => curr.north
        case 'v' => curr.south
        case '>' => curr.east
        case '<' => curr.west
      dropoffs += curr
  dropoffs.size

def part2(input: String): Long =
  val (santa, robo) = input.zipWithIndex.partition(_._2 % 2 == 0)
  def dropoffs(s: String) =
    val res = HashSet[Point]()
    val source = Point(0, 0)
    res += source
    var curr = source
    s.foreach:
      c =>
        curr = c match
          case '^' => curr.north
          case 'v' => curr.south
          case '>' => curr.east
          case '<' => curr.west
        res += curr
    res
  dropoffs(santa.map(_._1).mkString).union(dropoffs(robo.map(_._1).mkString)).size

@main def main() =
  val input = io.Source.fromFile("input.txt").getLines().mkString("\n").trim()
  println(s"2015 Day 3, Part 1: ${part1(input)}")
  println(s"2015 Day 3, Part 2: ${part2(input)}")
