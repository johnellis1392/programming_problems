package aoc2015

import common.run
import common.runPart1

object Day01 : aoc2015.Base<String, Long> {
  override val day: String get() = "1"

  override fun parseInput(input: String): String =
    input.trim()

  override fun part1(input: String): Long =
    input.sumOf {
      when (it) {
        '(' -> 1L
        ')' -> -1L
        else -> throw IllegalArgumentException("Illegal character: '$it'")
      }
    }

  override fun part2(input: String): Long =
    input.scan(0L) { r, c ->
      when (c) {
        '(' -> r + 1L
        ')' -> r - 1L
        else -> throw IllegalArgumentException("Illegal character: '$c'")
      }
    }.takeWhile { it >= 0 }.size.toLong()
}

fun main() {
  run(Day01)
}
