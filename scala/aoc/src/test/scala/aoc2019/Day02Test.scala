package aoc2019

class Day02Test extends Aoc2019TestBase[Day02.type]:
  override val day = Day02

  val input =
    """
      |1,9,10,3,2,3,11,0,99,30,40,50
      |""".stripMargin

  describe("part 1"):
    val expected = 3500
    it(s"part1($input) = $expected"):
      assert(expected == part1(input))

  describe("eval test"):
    List(
      ("1,9,10,3,2,3,11,0,99,30,40,50", "3500,9,10,70,2,3,11,0,99,30,40,50"),
      ("1,0,0,0,99", "2,0,0,0,99"),
      ("2,3,0,3,99", "2,3,0,6,99"),
      ("2,4,4,5,99,0", "2,4,4,5,99,9801"),
      ("1,1,1,4,99,5,6,0,99", "30,1,1,4,2,5,6,0,99"),
    ).map:
      (input, expected) =>
        it(s"eval($input) = $expected"):
          assert(expected == (day.parse(input) |> day.eval |> ((_:day.Input).mkString(","))))

//  describe("part 2"):
//    val expected = 0
//    it(s"part2($input) = $expected"):
//      assert(expected == part2(input))
