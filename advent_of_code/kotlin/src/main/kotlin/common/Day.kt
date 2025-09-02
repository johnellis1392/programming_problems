package common

import org.example.common.inputPathFor
import kotlin.io.path.readText

interface Day<Input, Output> {
  val year: String
  val day: String

  fun readInput(): String =
    inputPathFor(day, year).readText()

  fun parseInput(input: String): Input

  fun part1(input: Input): Output

  fun part2(input: Input): Output
}

fun <I, O, D : Day<I, O>> runPart1(day: D) =
  runPart1(day, day.readInput())

fun <I, O, D : Day<I, O>> runPart1(
  day: D,
  input: String = (day.readInput()),
) = runPart1(day, day.parseInput(input))

fun <I, O, D : Day<I, O>> runPart1(day: D, input: I) {
  println(
    """
      ${day.year}, Day ${day.day}, Part 1: ${day.part1(input)}
    """.trimIndent()
  )
}

fun <I, O, D : Day<I, O>> runPart2(
  day: D,
  input: String = day.readInput(),
) = runPart2(day, day.parseInput(input))

fun <I, O, D : Day<I, O>> runPart2(day: D, input: I) {
  println(
    """
      ${day.year}, Day ${day.day}, Part 2: ${day.part2(input)}
    """.trimIndent()
  )
}

fun <I, O, D : Day<I, O>> run(day: D) {
  val inputString = day.readInput()
  runPart1(day, inputString)
  runPart2(day, inputString)
}
