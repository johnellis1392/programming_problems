package aoc2015

def part1(input: String): Long =
  input.map:
    case '(' => 1
    case ')' => -1
  .sum

def part2(input: String): Long =
  input.map:
    case '(' => 1
    case ')' => -1
  .scan(0)(_ + _)
  .takeWhile(_ >= 0)
  .size

def main =
  val input = io.Source.fromFile("input/aoc2015/day01.input.txt").getLines().mkString.trim()
  println(s"2015 Day 1, Part 1: ${part1(input)}")
  println(s"2015 Day 1, Part 2: ${part2(input)}")
