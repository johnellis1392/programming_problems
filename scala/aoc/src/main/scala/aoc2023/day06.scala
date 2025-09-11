package aoc2023


val testInput = """
Time:      7  15   30
Distance:  9  40  200
"""


def part1(input: String): Long = 0L

def part2(input: String): Long = 0L

@main def main() =
  val debug = true
  val input = if debug
    then testInput
    else io.Source.fromFile("input.txt").getLines().mkString("\n")
  println(s"2023 Day 6, Part 1: ${part1(input)}")
  println(s"2023 Day 6, Part 2: ${part2(input)}")
