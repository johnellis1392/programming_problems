package aoc2023

import scala.collection.mutable.Queue

object Day15 {
  def readInput(input: String): List[String] = {
    input.trim().split(",").toList
  }

  def hash(code: String): Long = {
    code.map(_.toLong).foldLeft(0L) {
      (acc, v) =>
        ((acc + v) * 17) % 256
    }
  }

  def part1(input: String): Long = readInput(input).map(hash).sum

  def dump(boxes: Array[Queue[(String, Int)]]): Unit = {
    boxes.zipWithIndex.foreach { case (q, i) =>
      if (q.length > 0) {
        print(s"Box $i: ")
        q.foreach { case (label, focalLength) => print(s"[$label $focalLength] ")}
        println()
      }
    }
    println()
  }

  def part2(input: String): Long = {
    val codes = readInput(input)
    val boxes = Array.fill(256) { Queue[(String, Int)]() }
    val put_re = """^([a-zA-Z]+)=([0-9]+)$""".r
    val rm_re = """^([a-zA-Z]+)-$""".r

    codes.foreach { code =>
      // println(s"After '$code':")
      code match {
        case put_re(label, focalLength) =>
          val box = boxes(hash(label).toInt)
          box.map(_._1).zipWithIndex.find { case (lensLabel, _) => lensLabel == label } match {
            case Some((_, index)) => box(index) = (label, focalLength.toInt)
            case None => box.append((label, focalLength.toInt))
          }
        case rm_re(label) =>
          val box = boxes(hash(label).toInt)
          box.removeFirst { case (lensLabel, _) => lensLabel == label }
        case s =>
      }
      // dump(boxes)
    }

    // dump(boxes)

    boxes.zipWithIndex.flatMap { case (q, i) =>
      val boxId = i + 1
      q.zipWithIndex.map { case ((_, focalLength), j) =>
        val slotId = j + 1
        boxId * slotId * focalLength
      }
    }.sum
  }

  def main(args: Array[String]): Unit = {
    val filename = "input.txt"
    val testInput = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"
    val debug = false
    val input = if (debug) testInput else io.Source.fromFile(filename).getLines().mkString("\n")
    println(s"2023 Day 15, Part 1: ${part1(input)}")
    println(s"2023 Day 15, Part 2: ${part2(input)}")
  }
}
