package aoc2017

import common.{AocEvents, Day, DayTest}

trait Aoc2017TestBase[D <: Day] extends DayTest[D]:
  override val event = AocEvents.Aoc2017

