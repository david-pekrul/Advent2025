package day7

import helpers.Coord

object Day7 {
  def main(args: Array[String]): Unit = {
    //val input = helpers.Helpers.readTestFile(this)
    val input = helpers.Helpers.readFile(this)

    val coordArrays = input.zipWithIndex.map{(line, y) => {
      y -> line.zipWithIndex.filter(_._1 == '^').map{(char,x) => {
        x -> char
      }}.map(_._1).toSet
    }}
      .filterNot(_._2.isEmpty)
      .toMap

//    printSplitters(coordArrays)

    val startX = input.head.toCharArray.zipWithIndex.find(_._1 == 'S').get._2

    val filteredSplitters = filterSpliters(coordArrays)
//    printSplitters(filteredSplitters)

    val part1 = filteredSplitters.map(_._2.size).sum + 1 //+1 for the top one
    println(s"Part 1: $part1")

  }


  //todo: no need to keep the char; drop to Set[Int]
  def filterSpliters(splitterLocations: Map[Int, Set[Int]]): Map[Int, Set[Int]] = {

    //starting from the bottom
    //for each splitter, take its location, one left, and one right;
    //with that 3-wide swath, go up.
    //once one of the coords exists, if it shares the same X coord as the start splitter, then that splitter is unreachable
    def isSplitterReachable(splitter: Coord): Boolean = {

      val swathX = Set(splitter.x-1, splitter.x, splitter.x+1)

      val nonEmptySwathY = Range(0, splitter.y).toSeq.reverse.to(LazyList).map(y => {
        val swathY = splitterLocations.getOrElse(y, Set()).intersect(swathX)
        swathY
      }).find(_.nonEmpty)

      if(nonEmptySwathY.isEmpty) {
        false
      } else if(nonEmptySwathY.get.contains(splitter.x)) {
        false
      } else {
        true
      }
    }

    val rowsBottomUp = splitterLocations.keys.toSeq.sorted.reverse

    val reachableSplitters = rowsBottomUp.map(y => {
      val reachableSplittersOnRow = splitterLocations(y).filter(x => {
          isSplitterReachable(Coord(x,y))
      })
      y -> reachableSplittersOnRow
    }).toMap

    reachableSplitters
  }


  def printSplitters(splitterLocations: Map[Int, Set[Int]]): Unit = {
    val maxY = splitterLocations.keySet.max
    val maxX = splitterLocations.map(_._2.maxOption.getOrElse(0)).max


    println("------------------------------\r\n\r\n")
    Range.inclusive(0,maxY).foreach(y => {
      Range.inclusive(0,maxX).foreach(x => {
        val itemAtCoord = splitterLocations.get(y).exists(_.contains(x))
        if(itemAtCoord) {
          print("^")
        } else {
          print('.')
        }
      })
      println()
    })

  }


}
