package aoc2023

import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.ArrayDeque
import scala.collection.mutable.PriorityQueue
import scala.util.boundary
import scala.math.max
import java.time.Instant
import java.time.Duration


val testInput = """
#.#####################
#.......#########...###
#######.#########.#.###
###.....#.>.>.###.#.###
###v#####.#v#.###.#.###
###.>...#.#.#.....#...#
###v###.#.#.#########.#
###...#.#.#.......#...#
#####.#.#.#######.#.###
#.....#.#.#.......#...#
#.#####.#.#.#########v#
#.#...#...#...###...>.#
#.#.#v#######v###.###v#
#...#.>.#...>.>.#.###.#
#####v#.#.###v#.#.###.#
#.....#...#...#.#.#...#
#.#########.###.#.#.###
#...###...#...#...#.###
###.###.#.###v#####v###
#...#...#.#.>.>.#.>.###
#.###.###.#.###.#.#v###
#.....###...###...#...#
#####################.#
"""

sealed trait Direction
case object North extends Direction
case object South extends Direction
case object East extends Direction
case object West extends Direction

case class Point(r: Int, c: Int):
  override def toString(): String = s"($r, $c)"
  def north = aoc2015.Point(r - 1, c)
  def south = aoc2015.Point(r + 1, c)
  def east = aoc2015.Point(r, c + 1)
  def west = aoc2015.Point(r, c - 1)
  def neighbors = Set(north, south, east, west)

case class Vertex(pos: aoc2015.Point, dir: Direction):
  override def toString(): String = s"(${pos.r}, ${pos.c}, $dir)"
  def north = Vertex(pos.north, North)
  def south = Vertex(pos.south, South)
  def east = Vertex(pos.east, East)
  def west = Vertex(pos.west, West)
  def neighbors = Set(north, south, east, west)

case class Grid(grid: Array[Array[String]]):
  val height = grid.size
  val width = grid.head.size
  val start = aoc2015.Point(0, grid.head.zipWithIndex.find(_._1 == ".").map(_._2).get)
  val end = aoc2015.Point(height - 1, grid.last.zipWithIndex.find(_._1 == ".").map(_._2).get)

  override def toString(): String =
    grid.map(_.mkString).mkString("\n")

  def apply(r: Int, c: Int) = grid(r)(c)
  def apply(p: aoc2015.Point) = grid(p.r)(p.c)
  def apply(v: Vertex) = grid(v.pos.r)(v.pos.c)
  def valid(p: aoc2015.Point) = 0 <= p.r && p.r < height && 0 <= p.c && p.c < width
  def valid(v: Vertex) = 0 <= v.pos.r && v.pos.r < height && 0 <= v.pos.c && v.pos.c < width

  def moveable(curr: Vertex) =
    if !valid(curr) then false
    else
      (apply(curr), curr.dir) match
        case (".", _) => true
        case ("^", North) => true
        case (">", East) => true
        case ("<", West) => true
        case ("v", South) => true
        // case ("#", _) => false
        case _ => false


def readInput(input: String) = Grid:
  input.trim().split("\n").map(_.trim()).filter(_.nonEmpty).map:
    line => line.map(_.toString()).toArray
  .toArray


def dump(grid: Grid, depths: HashMap[aoc2015.Point, Long]) =
  val render = Array.fill(grid.height) {
    Array.fill(grid.width) { "" }
  }
  for r <- 0 until grid.height
      c <- 0 until grid.width
    do
      if depths.contains(aoc2015.Point(r, c))
      then render(r)(c) = "%3d".format(depths(aoc2015.Point(r, c)))
      else render(r)(c) = "  " + grid(r, c)
  println {
    render.map(row => row.mkString).mkString("\n")
  }

def getNeighbors(grid: Grid, curr: Vertex, visited: Set[aoc2015.Point]) =
  curr.neighbors.filter:
    v =>
      !visited.contains(v.pos) && grid.moveable(v)

def part1(grid: Grid): Long =
  val depths = HashMap[aoc2015.Point, Int]()

  // Modified Depth-First Search;
  // Since we have strict paths we need to follow, and we
  // only need to branch at specific points, each iteration
  // we consume an entire path until we get to a branch and
  // then recurse.
  def dfs(start: Vertex, depth: Int, visited: Set[aoc2015.Point]): Long =
    if visited contains start.pos then 0L
    else if start.pos == grid.end then 
      depths(start.pos) = depth
      depth
    else
      depths(start.pos) = depth
      var curr = start
      var d = depth
      var v = visited
      var neighbors = getNeighbors(grid, curr, v)
      while neighbors.size == 1 do
        v += curr.pos
        curr = neighbors.head
        neighbors = getNeighbors(grid, curr, v)
        d += 1
        depths(curr.pos) = d
      if curr.pos == grid.end then d
      else if neighbors.isEmpty then 0L
      else neighbors.map(p => dfs(p, d + 1, v + curr.pos)).max

  // Breadth-First Search;
  // This is a lot simpler, but since the default BFS calculates
  // the shortest path, keep track of each branch's visited
  // points in their own sets. Fortunately the amount of data
  // here is about the same as the DFS, since we only really
  // have one item in the queue for each branch.
  def bfs =
    val queue = ArrayDeque[(Long, Vertex, Set[aoc2015.Point])]()
    queue += ((0, Vertex(grid.start, South), Set.empty))
    var res = 0L
    while queue.nonEmpty do
      val (depth, vertex, visited) = queue.removeHead()
      if vertex.pos == grid.end then
        res = max(res, depth)
      else
        for n <- vertex.neighbors
          if !visited.contains(n.pos) && grid.moveable(n)
        do queue += ((depth + 1, n, visited + vertex.pos))
    res


  // BFS or DFS; dealer's choice.
  dfs(Vertex(grid.start, South), 0, Set.empty)
  // bfs


def dumpBranches(grid: Grid, nodes: Set[aoc2015.Point]) =
  val render = Array.fill(grid.height) { Array.fill(grid.width) { "" } }
  for r <- 0 until grid.height
      c <- 0 until grid.width
  do render(r)(c) =
    if nodes.contains(aoc2015.Point(r, c))
      then "O"
      else grid(r, c)
  println { render.map(row => row.mkString("")).mkString("\n") }

def dumpBranches2(grid: Grid, branches: Set[aoc2015.Point], paths: Set[aoc2015.Point]) =
  val render = Array.fill(grid.height) { Array.fill(grid.width) { "" } }
  for r <- 0 until grid.height
      c <- 0 until grid.width
  do render(r)(c) =
    if paths.contains(aoc2015.Point(r, c)) then "."
    else if branches.contains(aoc2015.Point(r, c)) then "O"
    else "#"
  println {
    render.map(_.mkString).mkString("\n")
  }


// First problem was a simple graph search, but this
// is a meta graph search. We must construct a graph
// of all the conjunctions, with weighted edges of
// path lengths, then do a search over this simplified
// graph for the longest path. Dijkstra may come in
// handy there.
def part2(grid: Grid): Long =

  def getNeighbors(node: aoc2015.Point, visited: Set[aoc2015.Point]) =
    node.neighbors.filter:
      n => !visited.contains(n) && grid.valid(n) && grid(n) != "#"

  // Get all branching points; these are the nodes in our new graph.
  def getNodes: Set[aoc2015.Point] =
    val queue = ArrayDeque[aoc2015.Point]()
    val nodes = HashSet[aoc2015.Point]()
    val visited = HashSet[aoc2015.Point]()
    queue += grid.start
    while queue.nonEmpty do
      val curr = queue.removeHead()
      visited += curr
      val neighbors = curr.neighbors.filter(n => grid.valid(n) && grid(n) != "#")
      if curr == grid.start || curr == grid.end || neighbors.size > 2 then
        nodes += curr
      for n <- neighbors if !visited.contains(n) do
        queue += n
    nodes.toSet

  // Get the paths forming the edges connecting each branch; these are
  // our edges.
  def getEdges(nodes: Set[aoc2015.Point]): Map[aoc2015.Point, Set[(Long, aoc2015.Point)]] =
    val queue = ArrayDeque[(Long, aoc2015.Point)]()
    val visited = HashSet[aoc2015.Point]()
    val edges = HashMap[aoc2015.Point, HashSet[(Long, aoc2015.Point)]]()
    for node <- nodes do
      edges(node) = HashSet()

    for node <- nodes do
      queue += ((0L, node))
      while queue.nonEmpty do
        val (depth, curr) = queue.removeHead()
        if curr != node && nodes.contains(curr) then
          edges(node) += ((depth, curr))
          edges(curr) += ((depth, node))
        else
          visited += curr
          for neighbor <- getNeighbors(curr, visited.toSet) do
            queue += ((depth + 1, neighbor))
      queue.clear()
      visited.clear()

    edges.map((p, s) => p -> s.toSet).toMap


  println("Calculating nodes...")
  val nodes = getNodes
  println("Calculating edges...")
  val edges = getEdges(nodes)

  println(s"Found ${nodes.size} nodes and ${edges.values.map(_.size).sum} edges")

  // Now do a BFS through our new graph.
  // Note here that much like the BFS in part 1, we
  // can't just use a general visited set because
  // that will get us the shortest path.
  given Ordering[(Long, aoc2015.Point, Set[aoc2015.Point])] = Ordering.by((_: (Long, aoc2015.Point, Set[aoc2015.Point]))._1)
  val queue = PriorityQueue[(Long, aoc2015.Point, Set[aoc2015.Point])]()
  queue enqueue ((0L, grid.start, Set.empty))
  var res = 0L

  while queue.nonEmpty do
    val (dist, node, visited) = queue.dequeue
    if node == grid.end then
      res = max(res, dist)
    else
      for (weight, neighbor) <- edges(node)
        if !visited.contains(neighbor)
      do queue enqueue ((dist + weight, neighbor, visited + node))

  res


def time[R](f: => R) =
  val before = Instant.now()
  val r = f
  val after = Instant.now()
  val duration = Duration.between(before, after)
  println(s"Time: ${duration.toMillis}ms")
  r

@main def main() =
  val debug = false
  val input = if debug
    then testInput
    else io.Source.fromFile("input.txt").getLines().mkString("\n")
  val grid = readInput(input)

  val res1 = time:
    part1(grid)
  println(s"2023 Day 23, Part 1: ${res1}")
  val res2 = time:
    part2(grid)
  println(s"2023 Day 23, Part 2: ${res2}")
