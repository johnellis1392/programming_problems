package aoc2015


def part1(input: String) =
  input.trim().map:
    case '(' => 1
    case ')' => -1
    case _ => 0 // Just in case
  .sum

def part2(input: String) =
  input.trim().map:
    case '(' => 1
    case ')' => -1
    case _ => 0
  .scan(0)(_ + _)
  .takeWhile(_ >= 0)
  .size

@main def main() =
  val input = io.Source.fromFile("input.txt").getLines().mkString("\n")
  println(s"2015 Day 1, Part 1: ${part1(input)}")
  println(s"2015 Day 1, Part 2: ${part2(input)}")
