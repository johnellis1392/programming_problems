package aoc2015

import common.{AocEvents, Day, DayTest}

trait Aoc2015TestBase[D <: Day] extends DayTest[D]:
  override val event = AocEvents.Aoc2015
