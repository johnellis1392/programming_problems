package aoc2015

import java.security.MessageDigest
import scala.annotation.tailrec
import scala.util.boundary
import scala.concurrent.{Await, Future, ExecutionContext}
import scala.concurrent.duration.Duration

object MD5:
  val digester = MessageDigest.getInstance("MD5")
  def hash(s: String) =
    synchronized:
      digester.digest(s.getBytes).map("%02x".format(_)).mkString

@tailrec
def part1(key: String, n: Int = 0): Int =
  val hash = MD5.hash(key + n.toString)
  if hash.startsWith("00000")
    then n
    else part1(key, n + 1)

@tailrec
def part2(key: String, n: Int = 0): Int =
  val hash = MD5.hash(key + n.toString)
  if hash.startsWith("000000")
    then n
    else part2(key, n + 1)

def part2(key: String): Int =
  val chunkSize = 1_000_000
  given ExecutionContext = ExecutionContext.global
  val futures = (0 until 10).map:
    i =>
      val s = chunkSize * i
      val e = chunkSize * (i + 1) - 1
      Future:
        var res: Option[Int] = None
        boundary:
          for j <- s until e do
            val hash = MD5.hash(key + j.toString)
            if hash.startsWith("000000") then
              res = Some(j)
              boundary.break()
        res
  val res = Await.result(Future.sequence(futures), Duration.Inf)
  res.filter(_.isDefined).head.get



@main def main() =
  val key = "iwrupvqb"
  println(s"2015 Day 4, Part 1: ${part1(key)}")
  println(s"2015 Day 4, Part 2: ${part2(key)}")
