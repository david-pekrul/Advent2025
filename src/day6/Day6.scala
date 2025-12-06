package day6

import scala.annotation.tailrec

object Day6 {
  def main(args: Array[String]): Unit = {
    val start = System.nanoTime()
//    val input = helpers.Helpers.readTestFile(this)
    val input = helpers.Helpers.readFile(this)

    val part1 = solvePart1(input)
    println(s"Part 1: $part1")

    val part2 = solvePart2(input)
    println(s"Part 2: $part2")

    val end = System.nanoTime() - start
    println(s"Time: ${end / 1000.0 / 1000.0}ms")
  }

  def solvePart1(input: Seq[String]): Long = {
    val columns = input.flatMap(line => {
      line.trim.replaceAll("\\s+", " ").split(" ").toSeq.zipWithIndex
    }).groupMap(_._2)(_._1).values.toSeq
    columns.map(processColumn).sum
  }

  def processColumn(col: Seq[String]): Long = {
    val operation = col.last
    val start = operation match {
      case "+" => 0L
      case "*" => 1L
    }
    col.dropRight(1).map(_.toLong).foldLeft(start)((acc,next) => {
       operation match {
        case "+" => acc + next
        case "*" => acc * next
      }
    })
  }

  def processColumn(operation: String, col: Seq[Long]): Long = {
    val start = operation match {
      case "+" => 0L
      case "*" => 1L
    }
    col.foldLeft(start)((acc, next) => {
      operation match {
        case "+" => acc + next
        case "*" => acc * next
      }
    })
  }


  def solvePart2(input: Seq[String]): Long = {

    val opsToIndexes = input.last.toCharArray.zipWithIndex.filter(_._1 != ' ').toSeq

    val splitColumns = opsToIndexes.sliding(2,1).map{case Seq(a,b) => {
      b._2 - a._2
    }}.toSeq :+ 99 //buffer to get the last numbers

    @tailrec
    def splitLine(remainingLine: Seq[Char], splitAmounts: Seq[Int], acc: Seq[String] = Seq()): Seq[String] = {
      if(remainingLine.isEmpty) {
        return acc
      }

      val nextNumberString = remainingLine.take(splitAmounts.head).mkString
      splitLine(remainingLine.drop(splitAmounts.head),splitAmounts.tail,acc:+nextNumberString)
    }

    val allColumnSets = input.flatMap(line => {
      val input = line.toCharArray.toSeq
      splitLine(input, splitColumns).zipWithIndex
    }).groupMap(_._2)(_._1).values
    //So far, these seem to be grouped and put into the Seq value "in order"!!!

    val rotatedNumbers = allColumnSets.map(columnNumbersAndOp => {
      val operation = columnNumbersAndOp.last.trim
      val numberStrings = columnNumbersAndOp.dropRight(1).flatMap(_.toCharArray.zipWithIndex)
        .groupMap(_._2)(_._1).map{kv => kv._2.mkString.trim}
      (operation,numberStrings.filter(_.nonEmpty).map(_.toLong).toSeq)
    })

    val results = rotatedNumbers.map{ (op,nums) => processColumn(op,nums)}
    results.sum
  }
}
