package day1

import helpers.Helpers

import scala.annotation.tailrec;

object Day1 {
  def main(args: Array[String]): Unit = {
//    val rawLines = Helpers.readFile("src/day1/test.txt")
    val rawLines = Helpers.readFile("src/day1/day1.txt")

    val part1 = runOps(rawLines)
    println(s"Part 1: ${part1}")
    val part2 = runOps2(rawLines)
    println(s"Part 2: ${part2}")
  }

  def runOps(rawOps: Seq[String]): Long = {

    @tailrec
    def _run(ops: Seq[Operation], safe: Safe, zeroCount: Long): Long = {
      if(ops.isEmpty) {
        return zeroCount
      }

      val nextSafe =  ops.head.spin(safe)
      val nextCount = nextSafe.current match {
        case 0 => zeroCount + 1
        case _ => zeroCount
      }
      _run(ops.tail, nextSafe, nextCount)
    }

    val operations = rawOps.map(Operation(_))

    _run(operations, Safe(50), 0)
  }

  def runOps2(rawOps: Seq[String]): Long = {

    @tailrec
    def _run(ops: Seq[Operation], safe: Safe, zeroCount: Long): Long = {
      if (ops.isEmpty) {
        return zeroCount
      }

      val (nextSafe,passesZero) = ops.head.spin2(safe)
      _run(ops.tail, nextSafe, zeroCount + passesZero )
    }

    val operations = rawOps.map(Operation(_))

    _run(operations, Safe(50), 0)
  }

  case class Safe(current: Int) {}

  case class Operation(op: String) {

    val rex = """(\w)(\d+)""".r

    val rex(direction,rawAmount) = op
    val amount = Integer.parseInt(rawAmount)

    def spin(safe: Safe): Safe = {
      val modifier = direction match {
        case "L" => amount * -1
        case _ => amount
      }

      Safe((safe.current + modifier + 100)%100)
    }

    def spin2(input: Safe): (Safe,Long) = {
      val modifier = (direction match {
        case "L" => amount * -1
        case _ => amount
      }) % 100

      val cycles = amount / 100;

      val nextSafe = Safe((input.current + modifier + 100)%100)
      val passesZero = (input.current + modifier) <= 0 || (input.current + modifier) >= 100
      (nextSafe, cycles + (if(input.current != 0 && passesZero) then 1 else 0 ))
    }
  }
}