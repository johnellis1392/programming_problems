package aoc2019

import common.Day
import Iterator.iterate

//import scala.jdk.CollectionConverters._

object Day01 extends Aoc2019Base:
  override val id = "01"
  type Input = List[Int]
  type Output = Int

  override def parse(input: String): List[Int] =
    input.trim.linesIterator.map(_.toInt).toList

  override def part1(input: List[Int]): Int =
    input.map(_ / 3 - 2).sum()

  override def part2(input: List[Int]): Int =
    input.map { i =>
      iterate(i) { _ / 3 - 2 }
        .takeWhile(_ > 0)
        .drop(1)
        .sum
    }.sum


//@main
//def main(): Unit = Day.run(Day01)
