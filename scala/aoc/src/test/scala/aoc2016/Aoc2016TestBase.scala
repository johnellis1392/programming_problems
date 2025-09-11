package aoc2016

import common.{AocEvents, Day, DayTest}

trait Aoc2016TestBase[D <: Day] extends DayTest[D]:
  override val event = AocEvents.Aoc2016

