package aoc2021

import common.{AocEvents, Day, DayTest}

trait Aoc2021TestBase[D <: Day] extends DayTest[D]:
  override val event = AocEvents.Aoc2021

