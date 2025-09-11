package aoc2015

type Gift = (Long, Long, Long)

def part1(gifts: Array[Gift]): Long =
  gifts.map:
    case (l, w, h) =>
      val sides = List(l * w, l * h, w * h)
      2 * sides.sum + sides.min
  .sum

def part2(gifts: Array[Gift]): Long =
  gifts.map:
    case (l, w, h) =>
      val sides = List(l, w, h).sorted
      2 * sides.take(2).sum + sides.product
  .sum

def main =
  val gifts = io.Source.fromFile("input/aoc2015/day02.input.txt").getLines.map:
    line => 
      val Array(l, w, h) = line.split("x")
      (l.toLong, w.toLong, h.toLong)
  .toArray
  println(s"2015 Day 2, Part 1: ${part1(gifts)}")
  println(s"2015 Day 2, Part 2: ${part2(gifts)}")
