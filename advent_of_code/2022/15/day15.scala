import scala.collection.mutable.ArrayBuffer
import scala.math.{min, max}
import scala.collection.mutable.ArrayDeque
import scala.util.boundary, boundary.break


case class Point(x: Int, y: Int):
  override def toString(): String = s"($x, $y)"

  def manhattan(p: Point) = (x - p.x).abs + (y - p.y).abs
  def +(p: Point) = Point(x + p.x, y + p.y)
  def -(p: Point) = Point(x - p.x, y - p.y)


case class SensorReading(sensor: Point, beacon: Point):
  val radius = sensor.manhattan(beacon)

  override def toString(): String =
    s"(sensor=$sensor, beacon=$beacon, radius=$radius)"

  def render =
    val a = Array.fill(radius * 2 + 1) { Array.fill(radius * 2 + 1) { "." } }
    val origin = Point(radius, radius)
    for r <- 0 to radius do 
      for c <- 0 to radius - r do
        a(origin.y + r)(origin.x + c) = "#"
        a(origin.y + r)(origin.x - c) = "#"
        a(origin.y - r)(origin.x + c) = "#"
        a(origin.y - r)(origin.x - c) = "#"
    a(origin.y)(origin.x) = "S"
    val b = origin - (sensor - beacon)
    a(b.y)(b.x) = "B"
    a.zipWithIndex.map((row, i) => s"${"%3d".format(sensor.y - radius + i)} ${row.mkString("")}").mkString("\n")


extension(range: Range) 
  def overlaps(o: Range) =
    range.contains(o.start) || range.contains(o.end) ||
    o.contains(range.start) || o.contains(range.end)

  def combine(o: Range) =
    min(range.start, o.start) to max(range.end, o.end)

// extension(range: Range) def combine(o: Range) =
//   min(range.start, o.start) to max(range.end, o.end)



def readInput(input: String): Array[SensorReading] =
  val line_re = "^Sensor at x=(-?\\d+), y=(-?\\d+): closest beacon is at x=(-?\\d+), y=(-?\\d+)$".r
  input.trim().split("\n").map(_.trim()).filter(_.nonEmpty).map {
    case line_re(sx, sy, bx, by) => SensorReading(
      sensor=Point(sx.toInt, sy.toInt),
      beacon=Point(bx.toInt, by.toInt)
    )
  }.toArray


def flatten(ranges: List[Range]): List[Range] =
  val input = ArrayDeque[Range]()
  val output = ArrayDeque[Range]()
  input.addAll(ranges)
  var dirty = true
  while dirty do
    dirty = false
    while input.nonEmpty do
      var curr = input.removeHead()
      var i = 0
      while i < output.size do
        if output(i).overlaps(curr) then 
          dirty = true
          curr = curr.combine(output.remove(i))
        else i += 1
      output += curr
  output.toList


def detectedRanges(readings: Array[SensorReading], row: Int): List[Range] =
  flatten {
    readings.map { reading =>
      val Point(sx, sy) = reading.sensor
      val dy = (row - sy).abs
      if dy <= reading.radius then
        val dx = reading.radius - dy
        (sx - dx to sx + dx)
      else
        (0 until 0)
    }.filter(_.nonEmpty).toList
  }


def part1(input: String, row: Int): Long =
  val readings = readInput(input)
  val ranges = detectedRanges(readings, row)
  val detectedBeacons = readings.map(_.beacon)
    .filter(b => b.y == row && ranges.exists(r => r.contains(b.x)))
    .distinct
  ranges.map(_.size.toLong).sum - detectedBeacons.size


def part2(input: String): Long =
  val readings = readInput(input)
  val MAX = 4000000
  // val MAX = 20
  val defaultRange = (0 to MAX)
  var res = Point(-1, -1)
  val splitRanges = ArrayBuffer[List[Range]]()
  boundary {
    for r <- 0 to MAX do
      val drs = detectedRanges(readings, r)
      if drs.size == 2 then
        val x = drs.sortBy(r => r.start).head.end + 1
        res = Point(x, r)
        break()
  }
  res.x.toLong * 4000000L + res.y.toLong


@main def main(): Unit =
  val filename = "input.txt"
  val debug = false
  val testInput = """
  Sensor at x=2, y=18: closest beacon is at x=-2, y=15
  Sensor at x=9, y=16: closest beacon is at x=10, y=16
  Sensor at x=13, y=2: closest beacon is at x=15, y=3
  Sensor at x=12, y=14: closest beacon is at x=10, y=16
  Sensor at x=10, y=20: closest beacon is at x=10, y=16
  Sensor at x=14, y=17: closest beacon is at x=10, y=16
  Sensor at x=8, y=7: closest beacon is at x=2, y=10
  Sensor at x=2, y=0: closest beacon is at x=2, y=10
  Sensor at x=0, y=11: closest beacon is at x=2, y=10
  Sensor at x=20, y=14: closest beacon is at x=25, y=17
  Sensor at x=17, y=20: closest beacon is at x=21, y=22
  Sensor at x=16, y=7: closest beacon is at x=15, y=3
  Sensor at x=14, y=3: closest beacon is at x=15, y=3
  Sensor at x=20, y=1: closest beacon is at x=15, y=3
  """
  val input = if debug
    then testInput
    else io.Source.fromFile(filename).getLines().mkString("\n")

  val row = if debug
    then 10
    else 2000000

  // 5139103 too low
  println(s"2022 Day 15, Part 1: ${part1(input, row)}")

  // 108000000 too low
  // 1499039824 too low
  println(s"2022 Day 15, Part 2: ${part2(input)}")
