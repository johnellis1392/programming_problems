package common

import common.extensions.{ensure, |>}

import java.nio.file.{Files, Path, Paths}
import jellis.common.extensions.*


trait Day:
  val id: String
  val event: AocEvent
  def inputFilename: String = s"day$id.input.txt"
  def inputFile: Path = event.inputDir + inputFilename
  type Input
  type Output

  def getInput: String =
    this.inputFile.ensure(Files.exists(_))
      |> Files.readString
      |> ((_:String).trim)

  def parse(input: String): Input
  def part1(input: Input): Output
  def part2(input: Input): Output


object Day:
  val ProjectDir: Path = System.getProperty("user.dir").asPath
  val ResourceDir: Path = ProjectDir + "src/main/resources"

  def runPart1(day: Day, input: day.Input): Unit =
    val result = day.part1(input)
    println(s"${day.event.year}, Day ${day.id}, Part 1: $result")

  def runPart2(day: Day, input: day.Input): Unit =
    val result = day.part2(input)
    println(s"${day.event.year}, Day ${day.id}, Part 2: $result")

  def run(day: Day): Unit =
    val input = day.getInput |> day.parse
    runPart1(day, input)
    runPart2(day, input)

  def runIdempotent(day: Day): Unit =
    day.getInput |> day.parse |> (runPart1(day, _: day.Input))
    day.getInput |> day.parse |> (runPart2(day, _: day.Input))


