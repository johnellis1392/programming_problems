package aoc2019

import org.scalatest.matchers.should.Matchers.shouldEqual

class Day01Test extends Aoc2019TestBase[Day01.type]:
  override val day = Day01

  describe("part 1"):
    List(
      (List(12), 2),
      (List(14), 2),
      (List(1969), 654),
      (List(100756), 33583),
    ).map:
      (input, expected) =>
        it(s"part1($input) = $expected"):
          assert(expected == day.part1(input))

  describe("part 2"):
    List(
      (List(14), 2),
      (List(1969), 966),
      (List(100756), 50346),
    ).map:
      (input, expected) =>
        it(s"part2($input) = $expected"):
          assert(expected == day.part2(input))
