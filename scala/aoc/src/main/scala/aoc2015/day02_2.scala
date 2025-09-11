package aoc2015

import scala.math.min

def readInput(input: String): Array[(Long, Long, Long)] =
  input.trim().split("\n").map(_.trim()).filter(_.nonEmpty).map:
    line =>
      val Array(l, w, h) = line.split("x").map(_.toLong)
      (l, w, h)

def part1(gifts: Array[(Long, Long, Long)]): Long =
  gifts.map:
    case (l, w, h) =>
      val sides = Array(l * w, w * h, h * l)
      val ribbonLength = sides.min
      val wrappingPaper = 2 * sides.sum
      wrappingPaper + ribbonLength
  .sum


def part2(gifts: Array[(Long, Long, Long)]): Long =
  gifts.map:
    case (l, w, h) =>
      List(l, w, h).sorted.take(2).map(_ * 2).sum + (l * w * h)
  .sum


@main def main() =
  val debug = false
  val input = if debug
    then """
    2x3x4
    1x1x10
    """
    else io.Source.fromFile("input.txt").getLines().mkString("\n")
  val gifts = readInput(input)
  println(s"2015 Day 2, Part 1: ${part1(gifts)}")
  println(s"2015 Day 2, Part 2: ${part2(gifts)}")
