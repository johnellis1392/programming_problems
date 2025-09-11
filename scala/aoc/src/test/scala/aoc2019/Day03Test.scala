package aoc2019

class Day03Test extends Aoc2019TestBase[Day03.type]:
  override val day = Day03

  val input =
    """
      |R8,U5,L5,D3
      |U7,R6,D4,L4
      |""".stripMargin

  val testCases = List(
    (
      """
        |R8,U5,L5,D3
        |U7,R6,D4,L4
        |""".stripMargin,
      6,
      30
    ),
    (
      """
        |R75,D30,R83,U83,L12,D49,R71,U7,L72
        |U62,R66,U55,R34,D71,R55,D58,R83
        |""".stripMargin,
      159,
      610
    ),
    (
      """
        |R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
        |U98,R91,D20,R16,D67,R40,U7,R15,U6,R7
        |""".stripMargin,
      135,
      410
    )
  )


  describe("part 1"):
    testCases.map:
      (input, expected, _) =>
        it(s"part1($input) = $expected"):
          assert(expected == part1(input))


  describe("part 2"):
    testCases.map:
      (input, _, expected) =>
        it(s"part2($input) = $expected"):
          assert(expected == part2(input))
