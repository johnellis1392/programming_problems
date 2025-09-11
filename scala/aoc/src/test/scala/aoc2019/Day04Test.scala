package aoc2019

import Day04.split

class Day04Test extends Aoc2019TestBase[Day04.type]:
  override val day = Day04

  describe("Int.split"):
    List(
      (10, 2, List(1, 0)),
      (100, 3, List(1, 0, 0)),
      (12345, 5, List(1, 2, 3, 4, 5)),
      (1, 6, List(0, 0, 0, 0, 0, 1)),
    ).map:
      (input, numDigits, expected) =>
        it(s"$input.split($numDigits) = $expected"):
          assert(expected == input.split(numDigits).toList)

  describe("straightGrouping"):
    List(
      (111122.split(6), List((1, 4), (2, 2))),
      (112233.split(6), List((1, 2), (2, 2), (3, 2))),
      (123456.split(6), List((1, 1), (2, 1), (3, 1), (4, 1), (5, 1), (6, 1))),
    ).map:
      (input, expected) =>
        it(s"$input.straightGrouping == $expected"):
          assert(day.straightGrouping(input.iterator).toList == expected)

//  describe("part 1"):
//    List(
//      ("122345", true),
//      ("111123", true),
//      ("135679", false),
//      ("111111", true),
//      ("223450", false),
//      ("123789", false),
//    ).map:
//      (input, expected) =>
//        it(s"part1($input) = $expected"):
//          assert(expected == day.valid2(input))


//  describe("part 2"):
//    val expected = ???
//    it(s"part2($input) = $expected"):
//      assert(expected == part2(input))
