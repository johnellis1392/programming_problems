package aoc2019

import common.Day

import scala.collection.mutable

object Day05 extends Aoc2019Base:
  override val id = "05"
  override type Input = Array[Int]
  override type Output = Int

  override def parse(input: String): Input =
    input.trim.split(",").map(_.toInt)


  sealed trait Mode

  case object Immediate extends Mode

  case object Position extends Mode

  sealed trait Op

  case class Add(a: Mode, b: Mode, dest: Mode) extends Op

  case class Mul(a: Mode, b: Mode, dest: Mode) extends Op

  case object Read extends Op

  case object Write extends Op

  case object Halt extends Op

  //  case object Opcode(flags: (Boolean, Boolean, Boolean), op: Op):
  case object Opcode:
    def unapply(n: Int): Option[Op] =
      val modeA = if (n / 100) % 10 == 1 then Immediate else Position
      val modeB = if (n / 1000) % 10 == 1 then Immediate else Position
      val modeDest = if (n / 10000) % 10 == 1 then Immediate else Position
      val op: Op = n % 100 match
        case 1 => Add(modeA, modeB, modeDest)
        case 2 => Mul(modeA, modeB, modeDest)
        case 3 => Read
        case 4 => Write
        case 99 => Halt
      Some(op)


  override def part1(input: Input): Output =
    val inputQueue = mutable.Queue[Int](1)
    val outputQueue = mutable.Queue[Int]()
    val buffer = input.clone()
    var i = 0

    def read(n: Int, mode: Mode): Int = mode match
      case Immediate => n
      case Position => buffer(n)

    def write(n: Int, v: Int, mode: Mode): Unit = mode match
      case Immediate => // NO-OP
      case Position => buffer(n) = v

    while buffer(i) % 100 != 99 do
      buffer.slice(i, buffer.length).toList match
        case Opcode(Add(ma, mb, md)) :: a :: b :: dest :: _ =>
          i += 4
          val av = read(a, ma)
          val bv = read(b, mb)
          write(dest, av + bv, md)

        case Opcode(Mul(ma, mb, md)) :: a :: b :: dest :: _ =>
          i += 4
          val av = read(a, ma)
          val bv = read(b, mb)
          write(dest, av * bv, md)

        case Opcode(Read) :: dest :: _ =>
          i += 2
          val v = inputQueue.dequeue()
          write(dest, v, Position)

        case Opcode(Write) :: dest :: _ =>
          i += 2
          val v = read(dest, Position)
          outputQueue.enqueue(v)

//        case Opcode(Halt) :: _ =>
//          ???
//
//        case _ => ???

    println(outputQueue)
    outputQueue.find(_ > 0).get

  override def part2(input: Input): Output = ???


@main
def main(): Unit = Day.run(Day05)
