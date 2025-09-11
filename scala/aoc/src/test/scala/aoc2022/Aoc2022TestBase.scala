package aoc2022

import common.{AocEvents, Day, DayTest}

trait Aoc2022TestBase[D <: Day] extends DayTest[D]:
  override val event = AocEvents.Aoc2022

