package aoc2015

import org.scalatest.matchers.should.Matchers.shouldEqual

class Day01Test extends Aoc2015TestBase:
  override val day = Day01

  describe("part 1 test"):
    List(
      ("(((", 3),
      (")))", -3),
    ).map:
      (input, expected) =>
        it(s"part1($input) = $expected"):
          expected shouldEqual part1(input)

  describe("part 2 test"):
    List(
      (")", 1),
      ("()())", 5)
    ).map:
      (input, expected) =>
        it(s"part2($input) = $expected"):
          expected shouldEqual part2(input)
