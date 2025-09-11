package aoc2015

import common.Day
import common.DayTestBase
import org.junit.jupiter.api.DynamicTest.dynamicTest
import org.junit.jupiter.api.TestFactory

class Day01Test : DayTestBase<String, Long> {
  override val day: Day<String, Long>
    get() = aoc2015.Day01

  @TestFactory
  fun `test part 1`() = listOf(
    Pair("()", 0L),
    Pair("(()", 1L),
    Pair("())", -1L),
    Pair("(())", 0L),
    Pair("()()", 0L),
    Pair("(((", 3L),
    Pair("(()(()(", 3L),
    Pair("))(", -1L),
    Pair(")))", -3L),
    Pair(")())())", -3L),
  ).map { (input, output) ->
    testPart1Raw(input, output)
  }
}
