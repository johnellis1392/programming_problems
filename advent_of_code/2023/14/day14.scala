import scala.collection.mutable.HashMap

object Day14 {
  case class Point(r: Int, c: Int) { self =>
    def +(o: Point): Point = Point(self.r + o.r, self.c + o.c)
    def -(o: Point): Point = Point(self.r - o.r, self.c - c)
    def north: Point = self + Point(-1,  0)
    def south: Point = self + Point( 1,  0)
    def east:  Point = self + Point( 0,  1)
    def west:  Point = self + Point( 0, -1)
  }

  case class Grid(matrix: Array[Array[String]]) { self =>
    val height = self.matrix.length
    val width = self.matrix(0).length

    def apply(r: Int, c: Int): String = self.matrix(r)(c)
    def apply(p: Point): String = self.matrix(p.r)(p.c)
    def update(r: Int, c: Int, v: String): Unit = self.matrix(r)(c) = v
    def update(p: Point, v: String): Unit = self.matrix(p.r)(p.c) = v
    override def toString(): String = self.matrix.map(_.mkString).mkString("\n")

    val rocks: Array[Point] = self.matrix.zipWithIndex.flatMap {
      (row, r) =>
        row.zipWithIndex.filter((col, _) => col == "O").map((_, c) => Point(r, c))
    }
  
    def valid(p: Point): Boolean = 0 <= p.r && p.r < self.height && 0 <= p.c && p.c < self.width
    def moveable(to: Point): Boolean = self.valid(to) && self(to.r, to.c) == "."

    def tiltNorth(): Unit = {
      val indices = self.rocks.zipWithIndex.sortBy((p, _) => p.r).map((_, i) => i)
      for (i <- indices) {
        var current = self.rocks(i)
        var next = current
        while (self.moveable(next.north))
          next = next.north
        self(current) = "."
        self(next) = "O"
        self.rocks(i) = next
      }
    }

    def tiltWest(): Unit = {
      val indices = self.rocks.zipWithIndex.sortBy((p, _) => p.c).map((_, i) => i)
      for (i <- indices) {
        var current = self.rocks(i)
        var next = current
        while (self.moveable(next.west)) 
          next = next.west
        self(current) = "."
        self(next) = "O"
        self.rocks(i) = next
      }
    }

    def tiltSouth(): Unit = {
      val indices = self.rocks.zipWithIndex.sortBy((p, _) => p.r).map((_, i) => i).reverse
      for (i <- indices) {
        var current = self.rocks(i)
        var next = current
        while (self.moveable(next.south))
          next = next.south
        self(current) = "."
        self(next) = "O"
        self.rocks(i) = next
      }
    }

    def tiltEast(): Unit = {
      val indices = self.rocks.zipWithIndex.sortBy((p, _) => p.c).map((_, i) => i).reverse
      for (i <- indices) {
        var current = self.rocks(i)
        var next = current
        while (self.moveable(next.east))
          next = next.east
        self(current) = "."
        self(next) = "O"
        self.rocks(i) = next
      }
    }

    def runSpinCycle(): Unit = {
      self.tiltNorth()
      self.tiltWest()
      self.tiltSouth()
      self.tiltEast()
    }

    def load(): Int = self.rocks.map { case Point(r, _) => height - r }.sum
  }

  def readInput(input: String): Grid = Grid {
    input.trim().split("\n")
      .map(_.trim())
      .filter(!_.isEmpty())
      .map(line => line.map(_.toString()).toArray).toArray
  }

  def part1(input: String): Int = {
    val grid = readInput(input)
    grid.tiltNorth()
    grid.load()
  }

  def spinCycle(
    grid: Grid,
    numCycles: Int,
    i: Int = 0,
    cache: HashMap[String, Int] = HashMap()
  ): Int = {
    if (i >= numCycles) return grid.load()
    grid.runSpinCycle()
    val state = grid.toString()

    if (cache.contains(state)) {
      val prevIndex = cache(state)
      val cycleLength = i - prevIndex
      val cyclesRemaining = (numCycles - prevIndex) % cycleLength
      for (_ <- 0 until cyclesRemaining - 1) grid.runSpinCycle()
      grid.load()
    } else {
      cache(state) = i
      spinCycle(grid, numCycles, i + 1, cache)
    }
  }

  def part2(input: String): Int = {
    val grid = readInput(input)
    val numCycles = 1000000000
    spinCycle(grid, numCycles)
  }

  def main(args: Array[String]): Unit = {
    val filename = "input.txt"
    val debug = false
    val test_input = """
      O....#....
      O.OO#....#
      .....##...
      OO.#O....O
      .O.....O#.
      O.#..O.#.#
      ..O..#O..O
      .......O..
      #....###..
      #OO..#.... 
    """

    val input = if (debug) test_input else io.Source.fromFile(filename).getLines().mkString("\n")

    println(s"2023 Day 14, Part 1: ${part1(input)}")
    println(s"2023 Day 14, Part 2: ${part2(input)}")
  }
}
