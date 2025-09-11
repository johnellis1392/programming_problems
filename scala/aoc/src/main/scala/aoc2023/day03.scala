package aoc2023

import scala.collection.mutable.ArrayBuffer


case class Point(r: Int, c: Int):
  override def toString() = s"($r, $c)"
  def north = aoc2015.Point(r - 1, c)
  def south = aoc2015.Point(r + 1, c)
  def east = aoc2015.Point(r, c + 1)
  def west = aoc2015.Point(r, c - 1)
  def neighbors = List(
    north, south, east, west,
    north.east, north.west, south.east, south.west
  )

case class Grid(
  grid: Array[Array[String]],
  numbers: Array[(Long, Array[aoc2015.Point])],
  symbols: Array[aoc2015.Point]
):
  val height = grid.size
  val width = grid.head.size

  override def toString(): String = grid.map(_.mkString).mkString("\n")
  def apply(p: aoc2015.Point) = grid(p.r)(p.c)
  def apply(r: Int, c: Int) = grid(r)(c)
  def apply(r: Int) = grid(r)


def readInput(input: String) =
  val lines = input.trim().split("\n").map(_.trim()).filter(_.nonEmpty)
  val numbers = ArrayBuffer[(Long, Array[aoc2015.Point])]()
  val symbols = ArrayBuffer[aoc2015.Point]()
  for r <- 0 until lines.length do
    var line = lines(r).toList.zipWithIndex
    while line.nonEmpty do
      line match
        case Nil => // Skip
        case (c, i) :: _ if c.isDigit =>
          val s = line.takeWhile((c, _) => c.isDigit).map(_._1)
          val n = s.mkString.toLong
          val ps = s.indices.map(j => aoc2015.Point(r, i + j)).toArray
          line = line.drop(s.length)
          numbers += ((n, ps))
        case ('.', _) :: rest =>
          line = rest
        case (_, c) :: rest =>
          symbols += aoc2015.Point(r, c)
          line = rest
  val grid = lines.map(line => line.map(_.toString()).toArray).toArray
  Grid(grid, numbers.toArray, symbols.toArray)


def part1(grid: Grid): Long =
  val symbols = grid.symbols.toSet
  grid.numbers.filter:
    (n, points) =>
      points.flatMap(_.neighbors).toSet.diff(points.toSet)
        .intersect(symbols)
        .nonEmpty
  .map(_._1)
  .sum



def part2(grid: Grid): Long =
  grid.symbols.map:
    symbol =>
      val adjs = grid.numbers.filter:
        (_, points) =>
          symbol.neighbors.intersect(points).nonEmpty
      if adjs.size == 2
      then adjs.map(_._1).product
      else 0L
  .sum


@main def main() =
  val debug = false
  val input = if debug
    then """
    467..114..
    ...*......
    ..35..633.
    ......#...
    617*......
    .....+.58.
    ..592.....
    ......755.
    ...$.*....
    .664.598..
    """
    else io.Source.fromFile("input.txt").getLines().mkString("\n")

  val grid = readInput(input)
  println(s"2023 Day 3, Part 1: ${part1(grid)}")
  println(s"2023 Day 3, Part 2: ${part2(grid)}")
