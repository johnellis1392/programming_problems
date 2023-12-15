
object Day14 {
  case class Point(r: Int, c: Int) {
    def north = Point(r - 1, c)
    def south = Point(r + 1, c)
    def east  = Point(r, c + 1)
    def west  = Point(r, c - 1)
  }

  case class Grid(
    matrix: Array[Array[String]],
    c0: Int,
    c1: Int,
    r1: Int
  ) { self =>
    val height = self.matrix.length
    val width = self.matrix(0).length

    def apply(r: Int, c: Int): String = self.matrix(r)(c - c0)
    def apply(p: Point): String = self.matrix(p.r)(p.c - c0)
    def update(r: Int, c: Int, v: String): Unit = self.matrix(r)(c - c0) = v
    def update(p: Point, v: String): Unit = self.matrix(p.r)(p.c - c0) = v

    def valid(p: Point): Boolean = 0 <= p.r && p.r < height && c0 <= p.c && p.c <= c1

    def moveable(p: Point): Boolean = valid(p) && this(p) == "."

    override def toString(): String = {
      self.matrix.map(_.mkString("")).mkString("\n")
    }
  }

  def readInput(input: String, withPadding: Boolean = false): Grid = {
    var c0: Int = 500
    var c1: Int = 500
    var r1: Int = 0
    val paths = input.trim().split("\n").map(_.trim()).map { line =>
      line.split(" -> ").map { p => 
        val Array(col, row) = p.split(",")
        val c = col.toInt
        val r = row.toInt
        c0 = c0.min(c)
        c1 = c1.max(c)
        r1 = r1.max(r)
        Point(r, c)
      }
    }

    if (withPadding) {
      r1 += 2
      val width = 2 * r1 + 1
      c0 = 500 - (width / 2 + 1)
      c1 = 500 + (width / 2 + 1)
    }

    val matrix = (0 to r1).map { _ =>
      (0 to c1 - c0).map { _ => "." }.toArray
    }.toArray
    val grid = Grid(matrix, c0, c1, r1)

    paths.foreach { path =>
      (1 until path.length).foreach { i =>
        val source = path(i-1)
        val dest = path(i)
        val r0 = source.r.min(dest.r)
        val r1 = source.r.max(dest.r)
        val c0 = source.c.min(dest.c)
        val c1 = source.c.max(dest.c)
        for {
          r <- r0 to r1
          c <- c0 to c1
        } {
          grid(r, c) = "#"
        }
      }
    }

    if (withPadding) {
      for (c <- 0 to c1 - c0) {
        grid(r1, c + c0) = "#"
      }
    }

    grid(0, 500) = "+"
    grid
  }

  def part1(input: String): Int = {
    val grid = readInput(input)
    val origin = Point(0, 500)
    var res = 0
    var current = origin
    while (grid.valid(current)) {
      if (grid.moveable(current.south) || !grid.valid(current.south)) {
        current = current.south
      } else if (grid.moveable(current.south.west) || !grid.valid(current.south.west)) {
        current = current.south.west
      } else if (grid.moveable(current.south.east) || !grid.valid(current.south.east)) {
        current = current.south.east
      } else {
        res += 1
        grid(current) = "o"
        current = origin
      }
    }
    res
  }

  def part2(input: String): Int = {
    val grid = readInput(input, withPadding = true)
    val origin = Point(0, 500)
    var res = 0
    var current = origin
    while (grid(origin) != "o") {
      if (grid.moveable(current.south)) {
        current = current.south
      } else if (grid.moveable(current.south.west)) {
        current = current.south.west
      } else if (grid.moveable(current.south.east)) {
        current = current.south.east
      } else {
        res += 1
        grid(current) = "o"
        current = origin
      }
    }
    res
  }

  def main(args: Array[String]): Unit = {
    val filename = "input.txt"
    val debug = false
    val test_input = """
      498,4 -> 498,6 -> 496,6
      503,4 -> 502,4 -> 502,9 -> 494,9
    """
    val input = if (debug) test_input else io.Source.fromFile(filename).getLines().mkString("\n")
    println(s"2022 Day 14, Part 1: ${part1(input)}")
    println(s"2022 Day 14, Part 2: ${part2(input)}")
  }
}
