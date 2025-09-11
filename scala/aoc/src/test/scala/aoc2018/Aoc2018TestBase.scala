package aoc2018

import common.{AocEvents, Day, DayTest}

trait Aoc2018TestBase[D <: Day] extends DayTest[D]:
  override val event = AocEvents.Aoc2018

