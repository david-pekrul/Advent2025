package day7

import helpers.Coord
import helpers.Vector

object Day7 {
  def main(args: Array[String]): Unit = {
//    val input = helpers.Helpers.readTestFile(this)
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

    //15541455599 too low
    val part2 = findPathCount(coordArrays, startX)
    println(s"Part 2: $part2")

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

  def findPathCount(splitterLocations: Map[Int, Set[Int]], startX: Int) : Long = {

    val splitterSet = splitterLocations.flatMap(y => {
      y._2.map(x => Coord(x, y._1))
    }).toSet

    val startTachs = Map(Coord(startX, 0) -> 1L)

    val rows = Range(1,splitterLocations.keys.max+1)

    val finalTachs = rows.foldLeft(startTachs)((currentTachs,row) => {
      val tachsMovedDown = currentTachs.map(kv => Vector.DOWN.apply(kv._1) -> kv._2)
      val splitTachs = tachsMovedDown.map{case (tach,count) => {
        if(splitterSet.contains(tach)) {
          //tach hit a splitter
          Seq((Vector.LEFT.apply(tach) -> count), (Vector.RIGHT.apply(tach) -> count))
        } else {
          Seq(tach -> count)
        }
      }}.flatten
        .groupMap(_._1)(_._2)
        .map(kv => {
          kv._1 -> kv._2.sum
        })

      splitTachs
    })

    finalTachs.values.map(_.toLong).sum
  }

}
