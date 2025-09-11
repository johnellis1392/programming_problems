package aoc2023

import common.{AocEvents, Day, DayTest}

trait Aoc2023TestBase[D <: Day] extends DayTest[D]:
  override val event = AocEvents.Aoc2023

