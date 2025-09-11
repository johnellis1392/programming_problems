package common

import extensions.|>
import org.scalatest.funspec.AnyFunSpec

trait DayTest[D <: Day] extends AnyFunSpec:
  val event: AocEvent
  val day: D

  def part1(input: String): day.Output = day.parse(input) |> day.part1
  def part2(input: String): day.Output = day.parse(input) |> day.part2
