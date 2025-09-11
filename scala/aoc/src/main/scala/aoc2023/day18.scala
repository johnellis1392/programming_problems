package aoc2023

import scala.collection.mutable.HashSet
import scala.math.{min, max}
import scala.concurrent.{Future, ExecutionContext}
import scala.util.{Success, Failure}
import java.util.concurrent.{Executors, ForkJoinPool}
import scala.concurrent.duration._
import scala.concurrent._
import java.util.concurrent.atomic.AtomicLong

object Day18 {
  sealed trait Orientation
  case object Vertical extends Orientation
  case object Horizontal extends Orientation

  sealed trait Direction
  case object North extends Direction
  case object South extends Direction
  case object East extends Direction
  case object West extends Direction

  implicit object OrientationOrdering extends Ordering[Orientation] {
    def compare(a: Orientation, b: Orientation): Int = {
      (a, b) match {
        case (Vertical, Horizontal) => -1
        case (Horizontal, Vertical) => 1
        case (_, _) => 0
      }
    }
  }

  case class Point(r: Int, c: Int)

  case class Border(
     start: Point,
     end: Point,
     direction: Direction,
     orientation: Orientation
   ) { self =>
    val size = self.orientation match {
      case Vertical => end.r - start.r
      case Horizontal => end.c - start.c
    }

    def isOnRow(r: Int): Boolean = self.start.r <= r && r <= self.end.r
    def isVertical = self.orientation == Vertical
    def isHorizontal = self.orientation == Horizontal
  }

  def parseBorders(insts: Array[(Direction, Int)]): (HashSet[Border], Point, Point) = {
    var x0 = 0; var x1 = 0; var y0 = 0; var y1 = 0;

    val borders = HashSet[Border]()
    var cur = Point(0, 0)
    var next = cur
    for ((dir, steps) <- insts) {
      next = dir match {
        case North => Point(cur.r - steps, cur.c)
        case South => Point(cur.r + steps, cur.c)
        case East => Point(cur.r, cur.c + steps)
        case West => Point(cur.r, cur.c - steps)
      }
      val orientation = if (dir == North || dir == South) Vertical else Horizontal
      borders += Border(
        Point(min(cur.r, next.r), min(cur.c, next.c)),
        Point(max(cur.r, next.r), max(cur.c, next.c)),
        dir,
        orientation
      )
      cur = next

      x0 = min(x0, cur.c)
      x1 = max(x1, cur.c)
      y0 = min(y0, cur.r)
      y1 = max(y1, cur.r)
    }

    val start = Point(y0, x0)
    val end = Point(y1, x1)

    (borders, start, end)
  }

  def numFilledCells(borders: List[Border], inShape: Boolean = false): Long = borders match {
    case Nil => 0
    case Border(_, _, _, Vertical) :: Nil => 0
    case Border(p1, _, _, Vertical) :: (b2@Border(p2, _, _, Vertical)) :: rest =>
      // Parallel Borders
      if (inShape)
        numFilledCells(b2 :: rest, false)
      else
        p2.c - (p1.c + 1) + numFilledCells(b2 :: rest, true)
    case Border(_, _, d1, Vertical) :: Border(_, _, _, Horizontal) :: (b3@Border(_, _, d2, Vertical)) :: rest if d1 != d2 =>
      // U Border
      if (inShape)
        numFilledCells(b3 :: rest, false)
      else
        numFilledCells(rest, false)
    case Border(_, _, d1, Vertical) :: Border(_, _, _, Horizontal) :: (b3@Border(_, _, d2, Vertical)) :: rest if d1 == d2 =>
      // Z Border
      if (inShape)
        numFilledCells(b3 :: rest, true)
      else
        numFilledCells(b3 :: rest, false)
    case _ => 
      println(s"Encountered unmatched pattern list: $borders, inShape=$inShape")
      0
  }


  def part1(input: String): Long = {
    val (borders, start, end) = parseBorders {
      val inst_re = "(U|D|L|R) ([0-9]+) \\(#([a-fA-F0-9]{6})\\)".r
      input.trim().split("\n").map(_.trim()).filter(_.nonEmpty).map {
        case inst_re(d, s, _) =>
          val steps = s.toInt
          val dir = d match {
            case "U" => North
            case "D" => South
            case "R" => East
            case "L" => West
          }
          (dir, steps)
      }
    }

    var res: Long = 0
    for {
      r <- start.r to end.r
    } {
      val bs = borders.filter(_.isOnRow(r)).toList
        .sortBy(b => (b.start.c, b.orientation))
      var temp = numFilledCells(bs)
      res += temp
    }


    var numBorders = 0
    borders.foreach { border =>
      if (border.isHorizontal) {
        numBorders += border.end.c - border.start.c
      } else {
        numBorders += border.end.r - border.start.r
      }
    }

    res += numBorders
    res
  }

  def part2(input: String): Long = {
    val (borders, start, end) = parseBorders {
      val inst_re = "(?:U|D|L|R) [0-9]+ \\(#([a-fA-F0-9]{6})\\)".r
      input.trim().split("\n").map(_.trim()).filter(_.nonEmpty).map {
        case inst_re(color) =>
          val steps = Integer.parseInt(color.substring(0, 5), 16)
          val direction = color.charAt(5) match {
            case '0' => East
            case '1' => South
            case '2' => West
            case '3' => North
          }
          (direction, steps)
      }
    }

    var res: Long = 0
    for (Border(p1, p2, _, orientation) <- borders) {
      if (orientation == Horizontal) {
        res += p2.c - p1.c
      } else {
        res += p2.r - p1.r
      }
    }

    for (r <- start.r to end.r) {
      val bs = borders.filter(_.isOnRow(r)).toList.sortBy(b => (b.start.c, b.orientation))
      val temp = numFilledCells(bs)
      res += temp
    }

    res
  }

  def part2Async(input: String): Long = {
    val (borders, start, end) = parseBorders {
      val inst_re = "(?:U|D|L|R) [0-9]+ \\(#([a-fA-F0-9]{6})\\)".r
      input.trim().split("\n").map(_.trim()).filter(_.nonEmpty).map {
        case inst_re(color) =>
          val steps = Integer.parseInt(color.substring(0, 5), 16)
          val direction = color.charAt(5) match {
            case '0' => East
            case '1' => South
            case '2' => West
            case '3' => North
          }
          (direction, steps)
      }
    }

    var res: Long = 0

    implicit val ec = ExecutionContext.global
    for (range <- (start.r to end.r).grouped(100)) {
      val f = Future.sequence {
        range.map { r =>
          Future {
            numFilledCells {
              borders.filter(_.isOnRow(r)).toList
                .sortBy(b => (b.start.c, b.orientation))
            }
          }
        }
      }
      res += Await.result(f, 5.seconds).sum
    }

    res
  }

  def main(args: Array[String]) = {
    val filename = "input.txt"
    val debug = false
    val testInput = """
      R 6 (#70c710)
      D 5 (#0dc571)
      L 2 (#5713f0)
      D 2 (#d2c081)
      R 2 (#59c680)
      D 2 (#411b91)
      L 5 (#8ceee2)
      U 2 (#caa173)
      L 1 (#1b58a2)
      U 2 (#caa171)
      R 2 (#7807d2)
      U 3 (#a77fa3)
      L 2 (#015232)
      U 2 (#7a21e3)
    """
    val input = if (debug) testInput else io.Source.fromFile(filename).getLines().mkString("\n")

    println(s"2023 Day 18, Part 1: ${part1(input)}")
    // println(s"2023 Day 18, Part 2: ${part2(input)}")
    println(s"2023 Day 18, Part 2: ${part2Async(input)}")
  }
}
