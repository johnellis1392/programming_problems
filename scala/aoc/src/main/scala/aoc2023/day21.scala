package aoc2023

import aoc2015.Point

import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayDeque


sealed trait Tile
case object G extends Tile: // Garden Plot
  override def toString(): String = "."
case object R extends Tile: // Rock
  override def toString(): String = "#"

case class Point(r: Int, c: Int):
  def north = aoc2015.Point(r - 1, c)
  def south = aoc2015.Point(r + 1, c)
  def east = aoc2015.Point(r, c + 1)
  def west = aoc2015.Point(r, c - 1)
  def neighbors = List(north, south, east, west)
  override def toString(): String = s"($r,$c)"

case class Grid(grid: Array[Array[Tile]]):
  val height = grid.length
  val width = grid(0).length

  def apply(r: Int, c: Int) = grid(r)(c)
  def apply(p: aoc2015.Point) = grid(p.r)(p.c)

  override def toString(): String =
    grid.map(row => row.map(_.toString()).mkString("")).mkString("\n")

  def valid(p: aoc2015.Point) = 0 <= p.r && p.r < height && 0 <= p.c && p.c < width

  def reframe(p: aoc2015.Point): aoc2015.Point =
    var r = p.r
    var c = p.c
    if r < 0 then r = height - (r.abs % height)
    if c < 0 then c = width - (c.abs % width)
    if r >= height then r = r % height
    if c >= width then c = c % width
    aoc2015.Point(r, c)


def readInput(input: String): (Grid, aoc2015.Point) =
  var start: aoc2015.Point = aoc2015.Point(-1, -1)
  val tiles = input.trim().split("\n").map(_.trim()).filter(_.nonEmpty)
    .zipWithIndex
    .map { (row, r) =>
      row.zipWithIndex.map { (v, c) =>
        v match
          case '.' => G
          case '#' => R
          case 'S' =>
            start = aoc2015.Point(r, c)
            G
      }.toArray
    }.toArray
  (Grid(tiles), start)


def dumpPoints(grid: Grid, points: HashSet[aoc2015.Point]): Unit =
  val render = Array.fill(grid.height) {
    Array.fill(grid.width) { "" }
  }
  for r <- 0 until grid.height
      c <- 0 until grid.width
    do render(r)(c) = grid(r, c).toString()
  for Point(r, c) <- points
    do render(r)(c) = "O"
  println {
    render.map(_.mkString("")).mkString("\n")
  }


def walk(grid: Grid, start: aoc2015.Point, steps: Int): Long =
  val queue = ArrayDeque[aoc2015.Point]()
  val points = HashSet[aoc2015.Point]()
  queue += start
  for _ <- 0 until steps do
    queue.addAll(points)
    points.clear()
    while queue.nonEmpty do
      val curr = queue.removeHead()
      // for n <- curr.neighbors if grid.valid(n) && grid(n) != R
      for n <- curr.neighbors if grid(grid.reframe(n)) != R
        do points += n
  points.size


def part1(input: String): Long =
  val (grid, start) = readInput(input)
  val steps = 64
  walk(grid, start, steps)



// I had no idea what polynomial fitting was before this, so
// this solution was shamefully stolen from this gist; all credit
// for this solution belongs to them:
// https://github.com/apprenticewiz/adventofcode/blob/main/2023/rust/day21b/src/main.rs
def part2(input: String, steps: Long): Long =
  val (grid, start) = readInput(input)

  val b0 = walk(grid, start, 65)
  val b1 = walk(grid, start, 65 + 131)
  val b2 = walk(grid, start, 65 + 2 * 131)
  val n = 202300L;

  val det_a: Double = -2.0;
  val det_a0: Double = -b0.toDouble + 2.0 * b1.toDouble - b2.toDouble
  val det_a1: Double = 3.0 * b0.toDouble - 4.0 * b1.toDouble + b2.toDouble
  val det_a2: Double = -2.0 * b0.toDouble

  val x0 = (det_a0 / det_a).toLong
  val x1 = (det_a1 / det_a).toLong
  val x2 = (det_a2 / det_a).toLong

  x0 * n * n + x1 * n + x2



@main def main(): Unit =
  val filename = "input.txt"
  val debug = false
  val testInput = """
    ...........
    .....###.#.
    .###.##..#.
    ..#.#...#..
    ....#.#....
    .##..S####.
    .##..#...#.
    .......##..
    .##.#.####.
    .##..##.##.
    ...........
  """
  val input = if debug
    then testInput
    else io.Source.fromFile(filename).getLines().mkString("\n")

  println(s"2023 Day 21, Part 1: ${part1(input)}")
  
  // for steps <- List(6)
  if debug then
    for steps <- List(6, 10, 50, 100, 500, 1000, 5000)
      do println(s"2023 Day 21, Part 2: steps=$steps, result=${part2(input, steps)}")
  else
    val totalSteps = 26501365
    println(s"2023 Day 21, Part 2: ${part2(input, totalSteps)}")
