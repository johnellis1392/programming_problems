package aoc2019

import common.Day

import scala.collection.mutable


object Day04 extends Aoc2019Base:
  override val id = "04"

  override def getInput: String = "234208-765869"

  override type Input = (Int, Int)
  override type Output = Int

  override def parse(input: String): Input =
    val Array(start, end) = input.trim.split("-").map(_.toInt)
    (start, end)

  extension (n: Int)
    def split(numDigits: Int): Array[Int] =
      {
        var m = n
        for
          i <- 1 to numDigits
        yield
          val res = m % 10
          m /= 10
          res
      }.reverse.toArray

  case class Password(ns: Array[Int]) { self =>
    def value: Int = self.ns.foldLeft(0) { (a, i) => a * 10 + i }
    def next: Password = Password.nextValidPassword(self.ns.clone())
    def valid: Boolean = Password.isValid(self.ns)

    override def toString: String = s"Password($value)"
  }

  object Password {
    def inc(ns: Array[Int]): Array[Int] =
      var i = 5
      while i >= 0 && ns(i) == 9 do
        i -= 1
      if i < 0 then throw Exception(s"Could not calculate next password for $ns")
      ns(i) += 1
      for j <- i until 6 do ns(j) = ns(i)
      ns

    def normalize(ns: Array[Int]): Array[Int] =
      var o: Option[Int] = None
      for i <- 1 until 6 do
        if ns(i) < ns(i-1) then o = Some(ns(i-1))
        ns(i) = o.getOrElse(ns(i))
      ns

    def nextValidPassword(ns: Array[Int]): Password =
      Password.inc(ns)
      Password.normalize(ns)
      if isValid(ns)
        then Password(ns)
        else nextValidPassword(ns)


    def nextValidPassword(n: Int): Password = nextValidPassword(n.split(6))

    def isValid(ns: Array[Int]): Boolean =
      ns.pairwise.foldLeft((false, true)) { case ((hasDouble, isIncreasing), (i, j)) =>
        (hasDouble || i == j, isIncreasing && i <= j)
      }.unzip { (hasDouble, isIncreasing) => hasDouble && isIncreasing }

    def from(n: Int): Password = Password(n.split(6))
  }

  def genValidPasswords(start: Int, end: Int): Iterator[Password] =
    val startingPassword = start |> Password.nextValidPassword
    Iterator.iterate(startingPassword) { p => p.next }
      .takeWhile(_.value <= end)

  override def part1(input: Input): Output =
    val (start, end) = input
    genValidPasswords(start, end).length

  def straightGrouping[T](it: Iterator[T]): List[(T, Int)] =
    val l = it.toList
    val res = mutable.ListBuffer[(T, Int)]()
    var count = 1
    var curr = l.head
    for i <- 1 until l.length do
      if l(i) == curr then
        count += 1
      else
        res += ((curr, count))
        curr = l(i)
        count = 1
    res += ((curr, count))
    res.toList

  override def part2(input: Input): Output =
    val (start, end) = input
    genValidPasswords(start, end).count { password =>
      straightGrouping(password.ns.iterator).count(_._2 == 2) >= 1
    }


//@main
//def main(): Unit = Day.run(Day04)
