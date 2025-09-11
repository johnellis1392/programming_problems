package aoc2023

object Day13 {
  type Grid = List[List[String]]
  def readInput(input: String): List[Grid] = {
    input.split("\n\n").map {
      grid =>
        grid.trim.split("\n").map(_.trim).filter(_.nonEmpty)
          .map(line => line.map(_.toString).toList).toList
    }.toList
  }

  def mirrorRow(grid: Grid): Int = {
    (1 until grid.length).filter { i =>
      grid.take(i).reverse.zip(grid.drop(i)).forall {
        (row1, row2) =>
          row1.zip(row2).forall { (c1, c2) => c1 == c2 }
      }
    }.headOption.getOrElse(0)
  }

  def mirrorRowWithSmudges(grid: Grid): Option[Int] = {
    (1 until grid.length).filter { i =>
      val numDifferences = grid.take(i).reverse.zip(grid.drop(i)).map { 
        (row1, row2) =>
          row1.zip(row2).count { (c1, c2) => c1 != c2 }
      }.sum
      numDifferences == 1
    }.headOption
  }

  def columns(grid: Grid): Grid = {
    grid(0).indices.map { i =>
      grid.map(line => line(i))
    }.toList
  }

  def part1(input: String): Int = {
    val grids = readInput(input)
    grids.map {
      grid =>
        100 * mirrorRow(grid) + mirrorRow(columns(grid))
    }.sum
  }

  def part2(input: String): Int = {
    val grids = readInput(input)
    grids.map {
      grid =>
        mirrorRowWithSmudges(grid).map(_ * 100)
          .orElse(mirrorRowWithSmudges(columns(grid)))
          .getOrElse(0)
    }.sum
  }

  def main(args: Array[String]): Unit = {
    val filename = "input.txt"
    val test_input = """
      #.##..##.
      ..#.##.#.
      ##......#
      ##......#
      ..#.##.#.
      ..##..##.
      #.#.##.#.

      #...##..#
      #....#..#
      ..##..###
      #####.##.
      #####.##.
      ..##..###
      #....#..#
    """
    val debug = false
    val input = 
      if (debug) test_input 
      else io.Source.fromFile(filename).getLines().mkString("\n")
    
    println(s"2023 Day 13, Part 1: ${part1(input)}")
    println(s"2023 Day 13, Part 2: ${part2(input)}")
  }
}
