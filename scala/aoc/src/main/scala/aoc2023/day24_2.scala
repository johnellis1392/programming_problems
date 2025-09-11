package aoc2023

import java.time.Instant
import java.time.Duration
import java.time.temporal.ChronoUnit

val testInput = """
19, 13, 30 @ -2,  1, -2
18, 19, 22 @ -1, -1, -2
20, 25, 34 @ -2, -2, -4
12, 31, 28 @ -1, -2, -1
20, 19, 15 @  1, -5, -3
"""

case class Point3(x: Double, y: Double, z: Double):
  override def toString(): String = s"($x, $y, $z)"
  def +(v: Vec3) = Point3(x + v.x, y + v.y, z + v.z)

case class Point2(x: Double, y: Double):
  override def toString(): String = s"($x, $y)"

case class Vec3(x: Double, y: Double, z: Double):
  override def toString(): String = s"<$x, $y, $z>"

case class Vec2(x: Double, y: Double):
  override def toString(): String = s"<$x, $y>"

case class Mat2(
  r00: Double, r01: Double,
  r10: Double, r11: Double
):
  def det = r00 * r11 - r01 * r10

// case class Mat3(r0: Vec3, r1: Vec3, r2: Vec3)

// p' = v * t + p
// (x, y) = (dx, dy) * t + (x, y)
// y = vy*t + y0
// x = vx*t + x0
case class Hailstone(pos: Point3, vel: Vec3):
  override def toString(): String = s"[$pos | $vel]"

  def intersection2D(o: Hailstone): Option[Point2] =
    val Point3(x1, y1, _) = pos
    val Point3(x2, y2, _) = pos + vel
    val Point3(x3, y3, _) = o.pos
    val Point3(x4, y4, _) = o.pos + o.vel

    val l1 = Mat2(x1, y1, x2, y2).det
    val l2 = Mat2(x3, y3, x4, y4).det
    val l1x = Mat2(x1, 1, x2, 1).det
    val l2x = Mat2(x3, 1, x4, 1).det
    val l1y = Mat2(y1, 1, y2, 1).det
    val l2y = Mat2(y3, 1, y4, 1).det
    val d = Mat2(
      l1x, l1y,
      l2x, l2y
    ).det

    if d == 0.0 then
      None
    else
      val nx = Mat2(
        l1, l1x,
        l2, l2x
      ).det
      val ny = Mat2(
        l1, l1y,
        l2, l2y
      ).det

      Some(Point2(nx / d, ny / d))

  def intersects2D(o: Hailstone): Boolean = intersection2D(o).isDefined

  def timeAt(p: Point2): Double =
    val Point3(_, y1, _) = pos
    val Point2(_, y2) = p
    val Vec3(_, vy, _) = vel
    // y' = vy * t + y0
    // =>
    // (y' - y0) / vy = t
    val t = (y2 - y1) / vy
    t


def readInput(input: String) =
  val line_re = "^(-?\\d+),\\s+(-?\\d+),\\s+(-?\\d+)\\s+@\\s+(-?\\d+),\\s+(-?\\d+),\\s+(-?\\d+)$".r
  input.trim().split("\n").map(_.trim()).filter(_.nonEmpty).map:
    case line_re(x, y, z, vx, vy, vz) =>
      Hailstone(
        Point3(x.toDouble, y.toDouble, z.toDouble),
        Vec3(vx.toDouble, vy.toDouble, vz.toDouble)
      )
  .toArray


def part1(hail: Array[Hailstone], start: Double, end: Double): Long =
  var res = 0L
  for i <- 0 until hail.size - 1
      j <- i + 1 until hail.size
  do
    val h1 = hail(i)
    val h2 = hail(j)
    h1.intersection2D(h2) match
      case None => // Skip
        // println("Don't intersect")
      case Some(p) =>
        if start <= p.x && p.x <= end && start <= p.y && p.y <= end then
          val t1 = h1.timeAt(p)
          val t2 = h2.timeAt(p)
          if t1 > 0 && t2 > 0 then
            res += 1L
            // println("Intersect in test area")
          // else
          //   println("Intersect in past")
        // else
        //   println("Outside test area")
  res


def part2(hail: Array[Hailstone], start: Long, end: Long): Long =
  0L


def time[R](f: => R) =
  val before = Instant.now()
  val res = f
  val after = Instant.now()
  val duration = Duration.between(before, after)
  val seconds = duration.getSeconds()
  val millis = duration.toMillis() % 1000L
  val nanos = duration.toNanos() % 1_000_000L
  if seconds > 0 then
    print(s"${seconds}s.")
  if millis > 0 then
    print(s"${millis}ms.")
  print(s"${nanos}ns")
  println()
  res


@main def main() =
  val debug = false
  val input = if debug
    then testInput
    else io.Source.fromFile("input.txt").getLines().mkString("\n")

  val (start, end) = if debug
    then (7L, 27L)
    else (200000000000000L, 400000000000000L)

  val hail = readInput(input)
  val res1 = time:
    part1(hail, start, end)
  // 12338 too low
  println
  println(s"2023 Day 24, Part 1: $res1")
  val res2 = time:
    part2(hail, start, end)
  println(s"2023 Day 24, Part 2: $res2")
  
