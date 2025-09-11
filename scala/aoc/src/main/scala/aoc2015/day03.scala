package aoc2015

case class Point(x: Long, y: Long):
  def north = Point(x, y + 1)
  def south = Point(x, y - 1)
  def east = Point(x + 1, y)
  def west = Point(x - 1, y)
  def move(c: Char) = c match
    case '^' => north
    case 'v' => south
    case '>' => east
    case '<' => west
    case _ => throw new Exception(s"unmatched character: $c")

def part1(input: String): Long =
  input.scanLeft(Point(0, 0)):
    (p, c) =>
      p.move(c)
  .toSet
  .size

def part2(input: String): Long =
  val (santaPath, robotPath) = input.zipWithIndex.partition(_._2 % 2 == 0)
  santaPath.map(_._1).scanLeft(Point(0, 0))(_.move(_))
  .toSet.union:
    robotPath.map(_._1).scanLeft(Point(0, 0))(_.move(_)).toSet
  .size

def main =
  val input = io.Source.fromFile("input/aoc2015/day03.input.txt").getLines.mkString
  println(s"2015 Day 3, Part 1: ${part1(input)}")
  println(s"2015 Day 3, Part 2: ${part2(input)}")
