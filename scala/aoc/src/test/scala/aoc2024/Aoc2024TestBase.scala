package aoc2024

import common.{AocEvents, Day, DayTest}

trait Aoc2024TestBase[D <: Day] extends DayTest[D]:
  override val event = AocEvents.Aoc2024

