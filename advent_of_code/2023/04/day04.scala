import scala.math.pow

case class Card(id: Int, winners: Set[Int], numbers: Set[Int]):
  override def toString(): String =
    s"Card $id: ${winners.map("%2d".format(_)).mkString(" ")} | ${numbers.map("%2d".format(_)).mkString(" ")}"

def readInput(input: String): Array[Card] =
  val line_re = "^Card\\s+(\\d+):\\s+(.*) \\| (.*)$".r
  input.trim().split("\n").map(_.trim()).filter(_.nonEmpty).map:
    case line_re(id, winners, numbers) =>
      Card(
        id.toInt,
        winners.trim().split("\\s+").map(_.toInt).toSet,
        numbers.trim().split("\\s+").map(_.toInt).toSet
      )


def part1(input: String): Long =
  readInput(input).map:
    card =>
      val n = card.winners.intersect(card.numbers).size
      pow(2, n - 1).toLong
  .sum


def part2(input: String): Long =
  val cards = readInput(input)
  val winningCards = Array.fill(cards.size) { 1L }
  for i <- cards.indices do
    val card = cards(i)
    val n = card.winners.intersect(card.numbers).size
    for j <- 1 to n do
      winningCards(i + j) += winningCards(i)
  winningCards.sum


@main def main() =
  val debug = false
  val input = if debug
    then """
    Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
    Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
    Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
    Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
    Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
    Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
    """
    else io.Source.fromFile("input.txt").getLines().mkString("\n")

  println(s"2023 Day 4, Part 1: ${part1(input)}")
  println(s"2023 Day 4, Part 2: ${part2(input)}")
