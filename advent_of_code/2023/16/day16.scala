import scala.collection.mutable
import scala.collection.mutable.ArrayDeque

object Day16 {
  sealed trait Direction
  case object North extends Direction
  case object South extends Direction
  case object East extends Direction
  case object West extends Direction

  case class Point(r: Int, c: Int) { self =>
    def north: Point = Point(r - 1, c)
    def south: Point = Point(r + 1, c)
    def east: Point = Point(r, c + 1)
    def west: Point = Point(r, c - 1)
    def move(dir: Direction): Point = dir match {
      case North => self.north
      case East => self.east
      case South => self.south
      case West => self.west
    }
  }

  case class Grid(matrix: Array[Array[String]]) { self =>
    val height = self.matrix.length
    val width = self.matrix.head.length

    override def toString(): String = {
      val matrixString = self.matrix.map(_.mkString("")).mkString("\n")
      s"Matrix:\n$matrixString\n".stripIndent()
    }

    def valid(p: Point): Boolean = 0 <= p.r && p.r < height && 0 <= p.c && p.c < width
    def apply(p: Point): String = self.matrix(p.r)(p.c)

    // p and dir are current point and direction, this returns the next point 
    // and direction. Assumes point is valid.
    def move(p: Point, dir: Direction): List[(Point, Direction)] = (self(p), dir) match {
      case ("/", North) => List((p.east, East))
      case ("/", South) => List((p.west, West))
      case ("/", East) => List((p.north, North))
      case ("/", West) => List((p.south, South))

      case ("\\", North) => List((p.west, West))
      case ("\\", South) => List((p.east, East))
      case ("\\", East) => List((p.south, South))
      case ("\\", West) => List((p.north, North))

      case ("-", North) | ("-", South) => List((p.east, East), (p.west, West))
      case ("-", East) | ("-", West) => List((p.move(dir), dir))

      case ("|", North) | ("|", South) => List((p.move(dir), dir))
      case ("|", East) | ("|", West) => List((p.north, North), (p.south, South))

      case (".", _) => List((p.move(dir), dir))
      case _ => List((p.move(dir), dir))
    }

  }

  def readInput(input: String): Grid = Grid {
    input.trim().split("\n").map(_.trim()).filter(!_.isEmpty())
      .map { line => line.map(_.toString()).toArray }.toArray
  }

  def energize(grid: Grid, origin: Point, originalHeading: Direction): Unit = {
  }

  def part1(input: String): Int = {
    val grid = readInput(input)
    val origin = Point(0, 0)
    val originalHeading = East
    val visited = mutable.Set[(Point, Direction)]()

    // Energize grid
    val queue = ArrayDeque[(Point, Direction)]((origin, originalHeading))
    while (!queue.isEmpty) {
      val (current, heading) = queue.removeHead()
      if (grid.valid(current) && !visited.contains((current, heading))) {
        visited.add((current, heading))
        queue.addAll(grid.move(current, heading))
      }
    }

    val energyTotal = visited.map { case (p, _) => p }.toSet.size
    energyTotal
  }

  def part2(input: String): Int = {
    val grid = readInput(input)
    val startingPoints = {
      (for (c <- 0 until grid.width) yield (Point(0, c), South)) ++
      (for (c <- 0 until grid.width) yield (Point(grid.height - 1, c), North)) ++
      (for (r <- 0 until grid.height) yield (Point(r, 0), East)) ++
      (for (r <- 0 until grid.height) yield (Point(r, grid.width - 1), West))
    }.toList

    var res = 0
    for ((origin, originalHeading) <- startingPoints) {
      val visited = mutable.Set[(Point, Direction)]()
      val queue = ArrayDeque[(Point, Direction)]((origin, originalHeading))
      while (!queue.isEmpty) {
        val (current, heading) = queue.removeHead()
        if (grid.valid(current) && !visited.contains((current, heading))) {
          visited.add((current, heading))
          queue.addAll(grid.move(current, heading))
        }
      }
      val energyTotal = visited.map { case (p, _) => p }.toSet.size
      res = math.max(res, energyTotal)
    }

    res
  }

  def main(args: Array[String]): Unit = {
    val filename = "input.txt"
    val debug = false
    val testInput = """
      >|<<<\....
      |v-.\^....
      .v...|->>>
      .v...v^.|.
      .v...v^...
      .v...v^..\
      .v../2\\..
      <->-/vv|..
      .|<<<2-|.\
      .v//.|.v..
    """
    val input = if (debug) testInput else io.Source.fromFile(filename).getLines().mkString("\n")
    println(s"2023 Day 16, Part 1: ${part1(input)}")
    println(s"2023 Day 16, Part 2: ${part2(input)}")
  }
}
