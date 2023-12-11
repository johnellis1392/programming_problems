object Day11 {
  case class Point(r: Int, c: Int) {}

  case class Grid(matrix: List[List[String]], expanse: Int = 2) { self =>
    val nullRows = self.matrix.zipWithIndex.flatMap { case (row, index) => 
      if (row.forall(_ == ".")) List(index)
      else Nil
    }.toSet

    val nullCols = self.matrix(0).indices.flatMap { col =>
      if (self.matrix.map(row => row(col)).forall(_ == ".")) List(col)
      else Nil
    }.toSet

    val galaxies = self.matrix.indices.flatMap { r =>
      matrix(r).indices.flatMap { c => 
        if (matrix(r)(c) == "#") List(Point(r, c))
        else Nil
      }
    }

    val height = matrix.size
    val width = matrix(0).size

    def dump(): Unit = {
      self.matrix.foreach { row => 
        println(row.mkString)
      }
    }

    def shortestPath(from: Point, to: Point): Int = {
      var current = from
      var res = 0
      while (current != to) {
        val dr = to.r - current.r
        val dc = to.c - current.c
        if (math.abs(dr) > math.abs(dc)) {
          if (nullRows contains current.r) res += self.expanse else res += 1
          current = Point(current.r + mag(dr), current.c)
        } else {
          if (nullCols contains current.c) res += self.expanse else res += 1
          current = Point(current.r, current.c + mag(dc))
        }
      }
      res
    }
  }

  def mag(i: Int): Int =
    if (i > 0) 1
    else if (i < 0) -1
    else 0

  def readInput(input: String): List[List[String]] = 
    input.split("\n").map(_.trim()).filter(_ != "")
      .map { line => line.map(_.toString()).toList }.toList

  def part1(input: String): Int = {
    val matrix = readInput(input)
    val grid = Grid(matrix)
    val paths = for {
      i <- 0 until grid.galaxies.size - 1
      j <- i + 1 until grid.galaxies.size
    } yield grid.shortestPath(grid.galaxies(i), grid.galaxies(j))
    paths.sum
  }

  def part2(input: String): Long = {
    val matrix = readInput(input)
    val grid = Grid(matrix, 1000000)
    val paths = for {
      i <- 0 until grid.galaxies.size - 1
      j <- i + 1 until grid.galaxies.size
    } yield grid.shortestPath(grid.galaxies(i), grid.galaxies(j))
    paths.map(_.toLong).sum
  }

  def main(args: Array[String]): Unit = {
    val DEBUG = false
    val filename = "input.txt"
    val input = if (DEBUG)
      """
        ...#......
        .......#..
        #.........
        ..........
        ......#...
        .#........
        .........#
        ..........
        .......#..
        #...#.....
      """.stripMargin.trim()
    else 
      io.Source.fromFile(filename).getLines().mkString("\n")
    
    println(s"2023 Day 11, Part 1: ${part1(input)}")
    println(s"2023 Day 11, Part 2: ${part2(input)}")
  }
}
