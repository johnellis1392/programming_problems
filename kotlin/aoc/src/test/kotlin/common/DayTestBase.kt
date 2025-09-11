package common

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.DynamicTest
import org.junit.jupiter.api.DynamicTest.dynamicTest

interface DayTestBase<I, O> {
  val day: Day<I, O>

  fun testPart1Raw(input: String, expected: O) = testPart1(day.parseInput(input), expected)

  fun testPart1(input: I, expected: O): DynamicTest =
    dynamicTest("Day ${day.day}, Part 1: $input should equal $expected") {
      val actual = day.part1(input)
      assertEquals(expected, actual) { "$actual != $expected" }
    }

  fun testPart2Raw(input: String, expected: O) = testPart2(day.parseInput(input), expected)

  fun testPart2(input: I, expected: O): DynamicTest =
    dynamicTest("Day ${day.day}, Part 2: $input should equal $expected") {
      val actual = day.part2(input)
      assertEquals(expected, actual) { "$actual != $expected" }
    }
}
