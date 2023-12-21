import scala.collection.mutable.PriorityQueue
import scala.collection.mutable.HashSet
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ArrayDeque


sealed trait Direction
case object North extends Direction
case object South extends Direction
case object East extends Direction
case object West extends Direction


case class Point(r: Int, c: Int):
  def north = Point(r - 1, c)
  def south = Point(r + 1, c)
  def east = Point(r, c + 1)
  def west = Point(r, c - 1)
  def move(dir: Direction) = dir match
    case North => north
    case South => south
    case East => east
    case West => west


case class Vertex(pos: Point, dir: Direction, depth: Int = 0):
  def turnLeft = dir match
    case North => Vertex(pos.west, West, 0)
    case South => Vertex(pos.east, East, 0)
    case East  => Vertex(pos.north, North, 0)
    case West  => Vertex(pos.south, South, 0)

  def turnRight = dir match
    case North => Vertex(pos.east, East, 0)
    case South => Vertex(pos.west, West, 0)
    case East  => Vertex(pos.south, South, 0)
    case West  => Vertex(pos.north, North, 0)

  def next = Vertex(pos.move(dir), dir, depth + 1)

  def neighbors(minSteps: Int, maxSteps: Int) =
    depth match
      case _ if depth < minSteps =>
        List(next)
      case _ if minSteps <= depth && depth < maxSteps =>
        List(turnLeft, turnRight, next)
      case _ =>
        List(turnLeft, turnRight)
    // if minSteps <= depth && depth < maxSteps
    //   then List(left, right, next)
    //   else List(left, right)


case class Grid(matrix: Array[Array[Int]]):
  val height = matrix.length
  val width = matrix(0).length

  def apply(r: Int, c: Int) = matrix(r)(c)
  def apply(p: Point) = matrix(p.r)(p.c)
  def apply(v: Vertex) = matrix(v.pos.r)(v.pos.c)

  def valid(p: Point): Boolean = 0 <= p.r && p.r < height && 0 <= p.c && p.c < width
  def valid(v: Vertex): Boolean = valid(v.pos)

  def points =
    for r <- 0 until height
        c <- 0 until width
      yield Point(r, c)

  override def toString() = 
    matrix.map(row => row.map(_.toString()).mkString("")).mkString("\n")


def readInput(input: String) = Grid {
  input.trim().split("\n")
    .map(_.trim())
    .filter(_.nonEmpty)
    .map(line => line.map(_.asDigit).toArray)
    .toArray
}


case class Cache[A](height: Int, width: Int, default: A):
  val cache: ArrayBuffer[ArrayBuffer[A]] = {
    val arr = ArrayBuffer[ArrayBuffer[A]]() 
    for _ <- 0 until height do 
      val row = ArrayBuffer[A]()
      for _ <- 0 until width do
        row += default
      arr += row
    arr
  }

  def apply(p: Point) = cache(p.r)(p.c)
  def apply(v: Vertex) = cache(v.pos.r)(v.pos.c)
  def update(p: Point, a: A) = cache(p.r)(p.c) = a
  def update(v: Vertex, a: A) = cache(v.pos.r)(v.pos.c) = a



// I feel like kind of an idiot, but I lost about
// 3 days of work on this because I was caching points
// in the visited set rather than vertices. Basically,
// points are not unique enough to count as "visited"
// since directions are also important in this problem,
// so we must cache vertices with direction and depth
// information as well.
def part1(input: String): Long =
  val grid = readInput(input)
  val origin = Point(0, 0)
  val destination = Point(grid.height - 1, grid.width - 1)
  val minSteps = 0
  val maxSteps = 2

  val queue = PriorityQueue.empty[(Long, Vertex)](
    Ordering.by((_: (Long, Vertex))._1).reverse)
  val visited = HashSet[Vertex]()

  queue += ((grid(origin.east), Vertex(origin.east, East, 1)))
  queue += ((grid(origin.south), Vertex(origin.south, South, 1)))

  while queue.nonEmpty do
    val (heat, curr) = queue.dequeue()
    if curr.pos == destination then
      return heat
    if !visited.contains(curr) then
      visited += curr
      for next <- curr.neighbors(minSteps, maxSteps) if grid.valid(next) do
        val newDist = heat + grid(next)
        queue += ((newDist, next))

  0L


def part2(input: String): Long =
  val grid = readInput(input)
  val origin = Point(0, 0)
  val destination = Point(grid.height - 1, grid.width - 1)
  val minSteps = 3
  val maxSteps = 9
  val queue = PriorityQueue.empty[(Long, Vertex)](
    Ordering.by((_: (Long, Vertex))._1).reverse)
  val visited = HashSet[Vertex]()

  queue += ((grid(origin.east), Vertex(origin.east, East, 1)))
  queue += ((grid(origin.south), Vertex(origin.south, South, 1)))

  while queue.nonEmpty do
    val (heat, curr) = queue.dequeue()
    if curr.pos == destination then
      return heat
    if !visited.contains(curr) then
      visited += curr
      for next <- curr.neighbors(minSteps, maxSteps) if grid.valid(next) do
        val newHeat = heat + grid(next)
        queue += ((newHeat, next))
  0L


@main def main(): Unit =
  val filename = "input.txt"
  val debug = false
  val testInput = """
    2413432311323
    3215453535623
    3255245654254
    3446585845452
    4546657867536
    1438598798454
    4457876987766
    3637877979653
    4654967986887
    4564679986453
    1224686865563
    2546548887735
    4322674655533
  """
  // val testInput = """
  // 111111111111
  // 999999999991
  // 999999999991
  // 999999999991
  // 999999999991
  // """
  val input = if (debug) testInput else io.Source.fromFile(filename).getLines().mkString("\n")

  println(s"2023 Day 17, Part 1: ${part1(input)}")
  println(s"2023 Day 17, Part 2: ${part2(input)}")
