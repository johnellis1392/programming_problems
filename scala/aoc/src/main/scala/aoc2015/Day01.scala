package aoc2015

import common.Day

object Day01 extends Aoc2015Base:
  override val id = "01"
  override type Input = List[Char]
  override type Output = Int

  override def parse(input: String): List[Char] =
    input.trim().toList

  override def part1(input: Input): Output =
    input.map:
      case '(' => 1
      case ')' => -1
    .sum

  override def part2(input: Input): Output =
    input.map:
      case '(' => 1
      case ')' => -1
    .scan(0)(_ + _)
    .takeWhile(_ >= 0)
    .length


@main
def main(): Unit = Day.run(Day01)

