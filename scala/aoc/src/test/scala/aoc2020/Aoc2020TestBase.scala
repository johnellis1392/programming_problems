package aoc2020

import common.{AocEvents, Day, DayTest}

trait Aoc2020TestBase[D <: Day] extends DayTest[D]:
  override val event = AocEvents.Aoc2020

