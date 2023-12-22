import scala.math.{min, max}
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ArrayDeque


case class Point(x: Int, y: Int, z: Int):
  override def toString(): String = s"($x, $y, $z)"
  def +(v: (Int, Int, Int)) =
    val (dx, dy, dz) = v
    Point(x+dx, y+dy, z+dz)


object Cube:
  private var labelId = 0

  def newLabel =
    val label = ('A' + labelId).toChar
    labelId += 1
    label.toString()

  def from(s: String) = 
    val line_re = "^(\\d+),(\\d+),(\\d+)~(\\d+),(\\d+),(\\d+)$".r
    s match
      case line_re(x1, y1, z1, x2, y2, z2) =>
        Cube(
          Point(x1.toInt, y1.toInt, z1.toInt),
          Point(x2.toInt, y2.toInt, z2.toInt)
        )
      case _ => throw new Exception(s"Invalid cube configuration: '$s'")

case class Cube(p1: Point, p2: Point, id: String = Cube.newLabel):
  override def toString(): String = s"Cube $id: $p1 ~ $p2"

  def xrange = p1.x to p2.x
  def yrange = p1.y to p2.y
  def zrange = p1.z to p2.z

  def topz = max(p1.z, p2.z)
  def bottomz = min(p1.z, p2.z)

  def xyplane = for x <- xrange; y <- yrange yield (x, y)
  def xyoverlaps(o: Cube) = xyplane.intersect(o.xyplane).nonEmpty
  def isUnder(o: Cube) = xyoverlaps(o) && p1.z < o.p1.z
  def supports(o: Cube) = isUnder(o) && (topz + 1) == o.bottomz
  def fall(n: Int) = Cube(p1 + (0, 0, -n), p2 + (0, 0, -n), id)


def readInput(input: String) =
  input.trim().split("\n").map(_.trim()).filter(_.nonEmpty).map(Cube.from)


def part1(input: Array[Cube]): Long =
  // Current highest z-point for a given (x, y) coord.
  // Defaults to 1.
  val heightMap = HashMap[(Int, Int), Int]()
  val cubes = input.sortBy(_.bottomz)
    .map:
      cube =>
        val xy = cube.xyplane
        val z = xy.map(heightMap.getOrElse(_, 0)).max
        val dz = cube.bottomz - (z + 1)
        val res = cube.fall(dz)
        for v <- xy do heightMap(v) = res.topz
        res

  val supports = HashMap[Cube, ArrayBuffer[Cube]](cubes.map(_ -> ArrayBuffer()):_*)
  val supportedBy = HashMap[Cube, ArrayBuffer[Cube]](cubes.map(_ -> ArrayBuffer()):_*)
  for i <- cubes.indices.init
      j <- i + 1 until cubes.size
    do
      val ci = cubes(i)
      val cj = cubes(j)
      if ci.supports(cj) then
        supports(ci) += cj
        supportedBy(cj) += ci

  cubes.count:
    cube =>
      supports(cube).forall:
        dep =>
          supportedBy(dep).size > 1


def part2(input: Array[Cube]): Long =
  val heightMap = HashMap[(Int, Int), Int]()
  val cubes = input.sortBy(_.bottomz)
    .map:
      cube =>
        val xy = cube.xyplane
        val z = xy.map(heightMap.getOrElse(_, 0)).max
        val dz = cube.bottomz - (z + 1)
        val res = cube.fall(dz)
        for v <- xy do heightMap(v) = res.topz
        res

  val supports = HashMap[Cube, ArrayBuffer[Cube]](cubes.map(_ -> ArrayBuffer()):_*)
  val supportedBy = HashMap[Cube, ArrayBuffer[Cube]](cubes.map(_ -> ArrayBuffer()):_*)
  for i <- cubes.indices.init
      j <- i + 1 until cubes.size
    do
      val ci = cubes(i)
      val cj = cubes(j)
      if ci.supports(cj) then
        supports(ci) += cj
        supportedBy(cj) += ci

  val keystones = supportedBy.filter((_, deps) => deps.size == 1).map(_._2.head).toSet
  
  var res = 0L
  for keystone <- keystones do
    val deps = HashSet[Cube]()
    val queue = ArrayDeque[Cube]()
    queue += keystone
    deps += keystone
    while queue.nonEmpty do
      val curr = queue.removeHead()
      for cube <- supports(curr) do
        if supportedBy(cube).forall(c => deps.contains(c))
        then deps += cube; queue += cube
    res += deps.size - 1
  
  res


@main def main() =
  val debug = false
  val input = if debug
    then """
    1,0,1~1,2,1
    0,0,2~2,0,2
    0,2,3~2,2,3
    0,0,4~0,2,4
    2,0,5~2,2,5
    0,1,6~2,1,6
    1,1,8~1,1,9
    """
    else io.Source.fromFile("input.txt").getLines().mkString("\n")

  val cubes = readInput(input)
  println(s"2023 Day 22, Part 1: ${part1(cubes)}")
  println(s"2023 Day 22, Part 2: ${part2(cubes)}")
