package common

import java.nio.file.Path
import extensions._

trait AocEvent:
  val year: String
  def inputDir: Path = Day.ResourceDir + s"aoc$year"


object AocEvents:
  object Aoc2015 extends AocEvent:
    override val year = "2015"

  object Aoc2016 extends AocEvent:
    override val year = "2016"

  object Aoc2017 extends AocEvent:
    override val year = "2017"

  object Aoc2018 extends AocEvent:
    override val year = "2018"

  object Aoc2019 extends AocEvent:
    override val year = "2019"

  object Aoc2020 extends AocEvent:
    override val year = "2020"

  object Aoc2021 extends AocEvent:
    override val year = "2021"

  object Aoc2022 extends AocEvent:
    override val year = "2022"

  object Aoc2023 extends AocEvent:
    override val year = "2023"

  object Aoc2024 extends AocEvent:
    override val year = "2024"
