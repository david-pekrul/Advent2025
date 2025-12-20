package day12

import helpers.Coord

import scala.annotation.tailrec

object Day12 {
  def main(args: Array[String]): Unit = {
//    val rawInput = helpers.Helpers.readTestFile(this)
    val rawInput = helpers.Helpers.readFile(this)

    val (presents,boxes) = parse(rawInput)

    val solution = solve(presents,boxes)
    println(s"Solution : $solution")
  }

  def solve(presents: Seq[Present], boxes: Seq[Box]): Int = {
    val boxToSizes = boxes.map(box => {
      box -> box.presentRequirements.zipWithIndex.map{case (boxCount,boxIdx) => {
        boxCount * presents(boxIdx).volume
      }}.sum
    })

    val boxesCapableOfWorking  = boxToSizes.filter { case (box,piecesVolume) => box.volume >= piecesVolume }

    val boxesThatDefinitelyWork = boxesCapableOfWorking.filter { case (box,_) => box.volume >= box.presentRequirements.map(_*9).sum}


    boxesThatDefinitelyWork.size
  }

  def parse(rawInput: Seq[String]): (Seq[Present], Seq[Box]) = {

    @tailrec
    def getPresents(remainingInput: Seq[String], allPresents: Seq[Present] = Seq.empty): (Seq[Present], Seq[String]) = {
      if(!remainingInput.head.matches("\\d:")) {
        return (allPresents, remainingInput)
      }

      val presentLines = remainingInput.takeWhile(line => line.nonEmpty)
      val presentIndex = presentLines.head.replace(":","").toInt

      val presentCoords = presentLines.tail.zipWithIndex.flatMap {case (line,y) => {
        line.toCharArray.zipWithIndex.filter(kv => kv._1 == '#').map{ case (c,x) => {
          Coord(x,y)
        }}
      }}.toSet

      val nextPresents = allPresents :+ Present(presentIndex, presentCoords)
      val nextLines = remainingInput.drop(presentLines.size+1)

      getPresents(nextLines, nextPresents)
    }

    val (presents, boxLines) = getPresents(rawInput)

    val boxes = boxLines.dropWhile(_.isBlank).map(line => {
      val split = line.split(":")
      val dimensions = split.head.split("x").map(_.toInt)
      val presentRequirements = split.last.trim.split(" ").map(_.toInt)
      Box(dimensions(0), dimensions(1), presentRequirements)
    })

    (presents, boxes)
  }

  case class Present(id: Int, coords: Set[Coord]) {
    lazy val volume: Int = coords.size
  }

  case class Box(width: Int, height: Int, presentRequirements: Seq[Int]) {
    lazy val volume: Int = width * height
  }
}
