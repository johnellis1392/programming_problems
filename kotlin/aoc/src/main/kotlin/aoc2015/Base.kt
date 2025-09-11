package aoc2015

import common.Day

interface Base<I, O> : Day<I, O> {
  override val year: String get() = "2015"

}
