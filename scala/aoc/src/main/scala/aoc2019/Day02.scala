package aoc2019

import common.Day

import scala.util.boundary.break

object Day02 extends Aoc2019Base:
  override val id = "02"
  override type Input = Array[Int]
  override type Output = Int

  override def parse(input: String): Input =
    input.trim.split(",").map(_.toInt)

  def eval(buffer: Array[Int]): Input =
    var i = 0
    while buffer(i) != 99 do
      buffer.slice(i, i + 4) match {
        case Array(1, a, b, dest) => buffer(dest) = buffer(a) + buffer(b)
        case Array(2, a, b, dest) => buffer(dest) = buffer(a) * buffer(b)
      }
      i += 4
    buffer

  override def part1(input: Input): Output =
    val buffer = input.clone()
    buffer(1) = 12
    buffer(2) = 2
    eval(buffer)
    buffer(0)

  override def part2(input: Input): Output =
    val exp = 19690720
    (for
      noun <- 0 to 99
      verb <- 0 to 99
    yield (noun, verb))
    .find:
      (noun, verb) =>
        val buffer = input.clone()
        buffer(1) = noun
        buffer(2) = verb
        eval(buffer)
        buffer(0) == exp
    match
      case Some((noun, verb)) => 100 * noun + verb
      case _ => throw Exception("Could not find value")


//@main
//def main(): Unit = Day.run(Day02)
