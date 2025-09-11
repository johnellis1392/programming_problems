package aoc2019

import common.{Day, Direction, East, North, Point2D, South, West, extensions}
import common.extensions.minOf

object Day03 extends Aoc2019Base:
  override val id = "03"
  override type Input = (WirePath, WirePath)
  override type Output = Int

  private type WirePath = List[(Direction, Int)]

  override def parse(input: String): Input =
    val Array(wire1, wire2) = input.trim.split("\n").map:
      wireString =>
        wireString.trim.split(",").map:
          case s"U$n" => (North, n.toInt)
          case s"D$n" => (South, n.toInt)
          case s"L$n" => (West, n.toInt)
          case s"R$n" => (East, n.toInt)
        .toList
    (wire1, wire2)


  override def part1(input: Input): Output =
    input.both { wire =>
        var curr = Point2D.origin
        for
          (d, n) <- wire
          _ <- 1 to n
        yield
          curr = curr.move(d)
          curr
      }
      .both(_.toSet)
      .unzip(_.intersect(_))
      .minOf(_.manhattanDistanceTo(Point2D.origin))


  override def part2(input: Input): Output =
    input.both { wire =>
        var curr = Point2D.origin
        var pathLength = 0
        for
          (d, n) <- wire
          _ <- 1 to n
        yield
          curr = curr.move(d)
          pathLength += 1
          curr -> pathLength
      }
      .both { extensions.toMapColliding(math.min) }
      .unzip { (wire1, wire2) =>
        wire1.keySet.intersect(wire2.keySet).map { p =>
          wire1(p) + wire2(p)
        }
      }.min


//@main
//def main(): Unit = Day.run(Day03)
