import scala.collection.mutable.ArrayDeque

val MAX = 4000
val defaultRange = (1 to 4000)
val emptyRange = (0 until 0)

object Component:
  def from(s: String) = s match 
    case "x" => X
    case "m" => M
    case "a" => A
    case "s" => S
    case v => throw new Exception(s"Unmatched Component name '$v'")

sealed trait Component
case object X extends Component
case object M extends Component
case object A extends Component
case object S extends Component

case class Part(x: Long, m: Long, a: Long, s: Long):
  def apply(c: Component) = c match
    case X => x
    case M => m
    case A => a
    case S => s

  def total = x + m + a + s

  override def toString(): String = s"{x=$x,m=$m,a=$a,s=$s}"


type PartRange = Map[Component, Range]

implicit class PartRangeUtils(ranges: PartRange):
  def total = ranges.values.map(_.size.toLong).product


implicit class RangeUtils(range: Range):
  def intersection(o: Range) =
    val s = range.intersect(o)
    if s.isEmpty
      then Range(0, 0)
      else (s.head to s.last)

  def inverse =
    val s = defaultRange.diff(range)
    if s.isEmpty
      then Range(0, 0)
      else (s.head to s.last)



sealed trait Result
case object Accept extends Result
case object Reject extends Result


sealed trait Rule:
  def eval(part: Part): Option[Result]
  def combos(partRange: PartRange): Long


case object AcceptRule extends Rule:
  override def eval(part: Part) = Some(Accept)
  override def toString() = "A"
  override def combos(partRange: PartRange) = partRange.total
    


case object RejectRule extends Rule:
  override def eval(part: Part) = Some(Reject)
  override def toString() = "R"
  override def combos(partRange: PartRange) = 0L


case class RangeRule(component: Component, range: Range, child: Rule) extends Rule:
  override def eval(part: Part) =
    if range.contains(part(component))
      then child.eval(part)
      else None

  override def toString() =
    val id = child match
      case Workflow(id, _) => id
      case e => e.toString()
    if range.start == 1
    then s"$component<${range.end}:$id"
    else s"$component>${range.start}:$id"

  override def combos(partRange: PartRange) =
    val m = partRange + (component -> range.intersection(partRange(component)))
    child.combos(m)


case class Workflow(id: String, rules: List[Rule]) extends Rule:
  override def eval(part: Part) =
    def applyRules(rules: List[Rule]): Option[Result] =
      rules match
        case Nil => None
        case rule :: rest =>
          rule.eval(part) match
            case Some(result) => Some(result)
            case None => applyRules(rest)
    applyRules(rules)

  override def toString() =
    s"$id{${rules.map(r => if r.isInstanceOf[Workflow] then r.asInstanceOf[Workflow].id else r.toString()).mkString(",")}}"

  override def combos(partRange: PartRange) = 
    var m = partRange
    var res = 0L
    for rule <- rules do
      res += rule.combos(m)
      rule match
        case RangeRule(c, range, _) =>
          m = m + (c -> range.inverse.intersection(m(c)))
        case _ => // Skip
    res


def readInput(input: String): (Rule, Array[Part]) =
  val Array(workflow_s, part_s) = input.trim().split("\n\n")
  val workflow_re = "^(\\w+)\\{(.*)\\}$".r
  val gt_rule = "^(x|m|a|s)>(\\d+):(A|R|\\w+)$".r
  val lt_rule = "^(x|m|a|s)<(\\d+):(A|R|\\w+)$".r

  val workflowStrings = workflow_s.trim().split("\n").map(_.trim()).filter(_.nonEmpty).map {
    case workflow_re(id, rules) => id -> rules
  }.toMap

  def build(id: String): Rule = id match
    case "A" => AcceptRule
    case "R" => RejectRule
    case _ =>
      val rules = workflowStrings(id).split(",").map {
        case gt_rule(c, t, id) => RangeRule(Component.from(c), (t.toInt + 1 to MAX), build(id))
        case lt_rule(c, t, id) => RangeRule(Component.from(c), (1 to t.toInt - 1), build(id))
        case id => build(id)
      }.toList
      Workflow(id, rules)
  
  val rule = build("in")

  val part_re = "^\\{x=(\\d+),m=(\\d+),a=(\\d+),s=(\\d+)\\}$".r
  val parts = part_s.trim().split("\n").map(_.trim()).filter(_.nonEmpty).map {
    case part_re(x, m, a, s) => Part(x.toLong, m.toLong, a.toLong, s.toLong)
  }
  
  (rule, parts)



def dumpRuleTree(rule: Rule): Unit =
  val q = ArrayDeque[Rule]()
  q += rule
  while q.nonEmpty do
    q.removeHead() match
      case RangeRule(_, _, child) => q += child
      case w@Workflow(id, rules) =>
        for rule <- rules do q += rule
        println(w)
      case _ => // Skip


def part1(input: String): Long = 
  val (rule, parts) = readInput(input)
  parts
    .filter(part => rule.eval(part) == Some(Accept))
    .map(_.total)
    .sum


def part2(input: String): Long = 
  val (rule, _) = readInput(input)
  rule.combos(List(X, M, A, S).map(_ -> defaultRange).toMap)


@main def main(): Unit = 
  val filename = "input.txt"
  val debug = false
  val testInput = """
    px{a<2006:qkq,m>2090:A,rfg}
    pv{a>1716:R,A}
    lnx{m>1548:A,A}
    rfg{s<537:gd,x>2440:R,A}
    qs{s>3448:A,lnx}
    qkq{x<1416:A,crn}
    crn{x>2662:A,R}
    in{s<1351:px,qqz}
    qqz{s>2770:qs,m<1801:hdj,R}
    gd{a>3333:R,R}
    hdj{m>838:A,pv}

    {x=787,m=2655,a=1222,s=2876}
    {x=1679,m=44,a=2067,s=496}
    {x=2036,m=264,a=79,s=2244}
    {x=2461,m=1339,a=466,s=291}
    {x=2127,m=1623,a=2188,s=1013}
  """
  val input = if (debug) testInput else io.Source.fromFile(filename).getLines().mkString("\n")

  println(s"2023 Day 19, Part 1: ${part1(input)}")
  println(s"2023 Day 19, Part 2: ${part2(input)}")
