package aoc2019

import common.{AocEvent, AocEvents, Day, DayTest}

trait Aoc2019TestBase[D <: Day] extends DayTest[D]:
  override val event: AocEvent = AocEvents.Aoc2019
