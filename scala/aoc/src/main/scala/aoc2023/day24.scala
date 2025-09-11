package aoc2023

import scala.annotation.tailrec

val testInput = """
19, 13, 30 @ -2,  1, -2
18, 19, 22 @ -1, -1, -2
20, 25, 34 @ -2, -2, -4
12, 31, 28 @ -1, -2, -1
20, 19, 15 @  1, -5, -3
"""

case class Point(x: Double, y: Double, z: Double):
  override def toString() = s"($x, $y, $z)"
  def +(v: Vector) = Point(x + v.x, y + v.y, z + v.z)

case class Vector(x: Double, y: Double, z: Double):
  override def toString() = s"<$x, $y, $z>"

case class Mat2(r0c0: Double, r0c1: Double, r1c0: Double, r1c1: Double):
  def det = r0c0 * r1c1 - r0c1 * r1c0

case class Hailstone(pos: Point, vel: Vector):
  override def toString() = s"[$pos, $vel]"

  def intersection(o: Hailstone): Option[Point] =
    val Point(p_ax, p_ay, _) = pos
    val Vector(v_ax, v_ay, _) = vel
    val Point(p_bx, p_by, _) = o.pos
    val Vector(v_bx, v_by, _) = o.vel
    val denominator = v_ay * v_bx - v_by * v_ax
    if denominator == 0.0 then
      None
    else
      val numerator = (p_by - p_ay) * v_bx * v_ax - p_bx * v_by * v_ax + p_ax * v_ay * v_bx
      val intersection_x = numerator / denominator
      val intersection_y = p_ay + (intersection_x - p_ax) * (v_ay / v_ax)
      Some(Point(intersection_x, intersection_y, 0.0))


def parseInput(input: String): Array[Hailstone] =
  val line_re = "^(-?\\d+),\\s*(-?\\d+),\\s*(-?\\d+)\\s*@\\s*(-?\\d+),\\s*(-?\\d+),\\s*(-?\\d+)$".r
  input.trim().split("\n").map(_.trim()).filter(_.nonEmpty).map:
    case line_re(x, y, z, vx, vy, vz) =>
      val p = Point(x.toDouble, y.toDouble, z.toDouble)
      val v = Vector(vx.toDouble, vy.toDouble, vz.toDouble)
      Hailstone(p, v)
  .toArray


def part1(hailstones: Array[Hailstone], bounds: (Double, Double)): Long =
  val (lower, upper) = bounds
  var res = 0L
  for
    i <- 0 until hailstones.size - 1
    j <- i + 1 until hailstones.size
  do
    val h1 = hailstones(i)
    val h2 = hailstones(j)
    h1.intersection(h2) match
      case None => // Lines don't intersect
      case Some(p@Point(x, y, _)) => // Lines intersect
        if lower <= x && x <= upper && lower <= y && y <= upper then
          val h1_in_past = (x < h1.pos.x) != (h1.vel.x < 0.0)
          val h2_in_past = (x < h2.pos.x) != (h2.vel.x < 0.0)
          if !h1_in_past && !h2_in_past then
            res += 1L
  res


// Idea: Take the first two hailstones, and for every possible
// time from t to infinity, increment the second hailstone by
// that time, and see if all other hailstones fall in line
// with it. This is tricky, because we'll be operating on a
// time distribution, but it should be possible. Basically we
// have to calculate the intersection point, get the time at
// that intersection point, and make sure that corresponds
// to our current velocity, for every other hailstone.
def part2(hailstones: Array[Hailstone]): Long =
  val h0 = hailstones(0)
  val h1 = hailstones(1)

  // p0, v0
  // p1, v1
  // t
  //
  // p1' = v1 * (t = 1) + p1
  // v_curr = p1' - p0
  //
  // pn' = vn * t + pn
  // v_n' = 

  // @tailrec
  // def loop(t: Long): Point =
  //   Point(0, 0, 0)
  
  // val p = loop(1L)
  // (p.x + p.y + p.z).toLong
  0L


def main =
  val debug = false
  val input = if debug
    then testInput
    else io.Source.fromFile("input/aoc2023/day24.input.txt").getLines.mkString("\n")

  val hailstones = parseInput(input)
  val bounds = if debug
    then (7.0, 27.0)
    else (200000000000000.0, 400000000000000.0)

  println(s"2023 Day 24, Part 1: ${part1(hailstones, bounds)}")
  println(s"2023 Day 24, Part 2: ${part2(hailstones)}")
