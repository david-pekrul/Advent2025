package day9

import helpers.{Coord, Helpers, Vector}

import scala.annotation.tailrec
import scala.util.boundary

object Day9_old {
  def main(args: Array[String]): Unit = {
    val start = System.nanoTime()
    //    val input = Helpers.readTestFile(this)
    val input = Helpers.readFile(this)

    val points = input.map(line => {
      line.split(",").map(_.toInt).toSeq
    })

    val part1 = solvePart1(points)
    println(s"Part 1: $part1")


    //4725826296 too high
    val part2 = solvePart2(points)
    println(s"Part 2: $part2")

    val end = System.nanoTime() - start
    println(s"Time: ${end / 1000.0 / 1000.0}ms")
  }

  def solvePart1(points: Seq[Seq[Int]]): Long = {

    points.map(_.map(_.toLong)).zipWithIndex.map { case (point, idx) => {
      val maxAreaForPoint = points.drop(idx + 1).foldLeft(0L)((acc, other) => {
        val area = (Math.abs(point.head - other.head) + 1) * (Math.abs(point.last - other.last) + 1)
        if (area > acc) {
          area
        } else {
          acc
        }
      })
      maxAreaForPoint
    }
    }.max
  }

  def solvePart2(points: Seq[Seq[Int]]): Long = {
    val (minX, maxX, minY, maxY) = points.foldLeft((Int.MaxValue, 0, Int.MaxValue, 0)) { case ((minX, maxX, minY, maxY), nextPoint) =>
      (Math.min(minX, nextPoint.head), Math.max(maxX, nextPoint.head), Math.min(minY, nextPoint.last), Math.max(maxY, nextPoint.last))
    }

    val topLeft = Coord(minX, minY)

    val coords = points.map { case Seq(x, y) => Coord(x, y) }

    val edges = coords.zipWithIndex.flatMap { case (a, idx) => {
      val b = coords((idx + 1) % coords.size) //enable the wrap around for the last point
      val v = a.vectorTo(b)
      val direction = v match {
        case Vector(x, 0) => {
          if x < 0 then Vector.LEFT else Vector.RIGHT
        }
        case Vector(0, y) => {
          if y < 0 then Vector.UP else Vector.DOWN
        }
      }
      val magnitude = Math.max(Math.abs(v.deltaX), Math.abs(v.deltaY))

      val edgesPoints = Range(0, magnitude).foldLeft(Set[Coord]())((acc, next) => {

        val edgePoint = direction.scale(next).apply(a)
        acc + edgePoint
      })

      edgesPoints
    }
    }.toSet
    //pick a point that CAN'T get to a 0-point
    val allEdgeAndCoords = edges ++ coords

    @tailrec
    def findEdge(lastCoord: Coord): Coord = {
      val nextCoord = Vector.DOWN_RIGHT.apply(lastCoord)
      if (allEdgeAndCoords.contains(nextCoord)) {
        return lastCoord
      }
      findEdge(nextCoord)
    }

    val outerPointInShell = findEdge(topLeft)

    @tailrec
    def constructShell(leadingEdge: Set[Coord], currentShell: Set[Coord]): Set[Coord] = {
      if (leadingEdge.isEmpty) {
        return currentShell
      }

      val nextLeadingEdge = leadingEdge
        .flatMap(_.neighbors())
        .filter(n => n.eightNeighbors().exists(allEdgeAndCoords.contains))
        .diff(allEdgeAndCoords)
        .diff(currentShell)

      constructShell(nextLeadingEdge, currentShell ++ leadingEdge)
    }

    val shell = constructShell(Set(outerPointInShell), Set())
    println("shell made")

    val shellMap = shell.map(c => c.y -> c.x).groupMap(_._1)(_._2)

    var skipCount = 0L;

    val overallMaxSize = coords.zipWithIndex.foldLeft((0L, Set[Coord]())) { case ((currentMaxSize, currentMaxPair), (coordA, aIndex)) => {

      println(s"aIndex = $aIndex \t currentMaxSize: $currentMaxSize \t $currentMaxPair")

      val (maxForA, maxPairforA) = coords.drop(aIndex + 1).foldLeft((currentMaxSize, currentMaxPair)) { case ((currentMaxForA, currentSetForA), coordB) => {

        val (minX, maxX) = if (coordA.x < coordB.x) {
          (coordA.x, coordB.x)
        } else {
          (coordB.x, coordA.x)
        }
        val (minY, maxY) = if (coordA.y < coordB.y) {
          (coordA.y, coordB.y)
        } else {
          (coordB.y, coordA.y)
        }

        val thisRectangleSize = (1 + maxX - minX).toLong * (1 + maxY - minY).toLong

        if (thisRectangleSize <= currentMaxForA) {
          skipCount += 1
          if(skipCount %100 ==0){
            println(s"skipCount: $skipCount")
          }
          (currentMaxForA, currentSetForA) //Not even worth checking, as it would be smaller anyways
        } else {
          //could possibly be bigger
          val topLeft = Coord(minX, minY)
          val bottomLeft = Coord(minX, maxY)
          val topRight = Coord(maxX, minY)
          val bottomRight = Coord(maxX, maxY)
          val edgesForPair = buildEdges(Seq(topRight, topLeft, bottomLeft, bottomRight), shell)
//          val edgesForPair = buildEdges2(Seq(topRight, topLeft, bottomLeft, bottomRight), shellMap)

          if(edgesForPair.isEmpty) {
            (currentMaxForA, currentSetForA)
          } else {
            if (thisRectangleSize > currentMaxForA) {
              (thisRectangleSize, Set(coordA, coordB))
            } else {
              (currentMaxForA, currentSetForA)
            }
          }
        }
      }
      }
      if (maxForA > currentMaxSize) {
        (maxForA, maxPairforA)
      } else {
        (currentMaxSize, currentMaxPair)
      }
    }
    }


    overallMaxSize._1
  }

  def distance(a: Seq[Int], b: Seq[Int]): Double = {
    val x = a.zip(b).map { case (c, d) => d - c }.map(Math.pow(_, 2)).sum
    Math.sqrt(x)
  }

  def printPoints(points: Set[Coord]): Unit = {
    val (minX, maxX, minY, maxY) = points.foldLeft((Int.MaxValue, 0, Int.MaxValue, 0)) { case ((minX, maxX, minY, maxY), nextPoint) =>
      (Math.min(minX, nextPoint.x), Math.max(maxX, nextPoint.x), Math.min(minY, nextPoint.y), Math.max(maxY, nextPoint.y))
    }

    println("=============================")

    Range.inclusive(minY, maxY).foreach(y => {
      val line = Range.inclusive(minX, maxX).map(x => {
        if (points.contains(Coord(x, y))) {
          "X"
        } else {
          "_"
        }
      }).mkString
      println(line)
    })
  }

  def buildEdges(coords: Seq[Coord], intersectEdge: Set[Coord] = Set()): Set[Coord] = {
    val edges = coords.zipWithIndex.flatMap { case (a, idx) => {
      val b = coords((idx + 1) % coords.size) //enable the wrap around for the last point
      val v = a.vectorTo(b)
      val direction = v match {
        case Vector(x, 0) => {
          if x < 0 then Vector.LEFT else Vector.RIGHT
        }
        case Vector(0, y) => {
          if y < 0 then Vector.UP else Vector.DOWN
        }
      }
      val magnitude = Math.max(Math.abs(v.deltaX), Math.abs(v.deltaY))

      val edgesPoints = Range(0, magnitude).foldLeft(Set[Coord]())((acc, next) => {

        val edgePoint = direction.scale(next).apply(a)

        if (intersectEdge.contains(edgePoint)) {
          return Set()
        }

        acc + edgePoint
      })

      edgesPoints
    }
    }.toSet
    //pick a point that CAN'T get to a 0-point
    val allEdgeAndCoords = edges ++ coords
    allEdgeAndCoords
  }

  def buildEdges2(coords: Seq[Coord], intersectEdge: Set[Coord]): Boolean = {
    coords.zipWithIndex.exists{case (a, idx) => {
      val b = coords((idx + 1) % coords.size) //enable the wrap around for the last point
      val v = a.vectorTo(b)
      val direction = v match {
        case Vector(x, 0) => {
          if x < 0 then Vector.LEFT else Vector.RIGHT
        }
        case Vector(0, y) => {
          if y < 0 then Vector.UP else Vector.DOWN
        }
      }
      val magnitude = Math.max(Math.abs(v.deltaX), Math.abs(v.deltaY))

      val edgeIntersects = Range(0, magnitude).exists(next => {
        val edgePoint = direction.scale(next).apply(a)
        intersectEdge.contains(edgePoint)
          false
      })

      edgeIntersects
    }}
  }

//  def buildEdges2(coords: Seq[Coord], intersectEdgeMap: Map[Int,Set[Int]] = Map()): Set[Coord] = {
//    val edges = coords.zipWithIndex.flatMap { case (a, idx) => {
//      val b = coords((idx + 1) % coords.size) //enable the wrap around for the last point
//      val v = a.vectorTo(b)
//      val direction = v match {
//        case Vector(x, 0) => {
//          if x < 0 then Vector.LEFT else Vector.RIGHT
//        }
//        case Vector(0, y) => {
//          if y < 0 then Vector.UP else Vector.DOWN
//        }
//      }
//      val magnitude = Math.max(Math.abs(v.deltaX), Math.abs(v.deltaY))
//
//      val edgesPoints = Range(0, magnitude).foldLeft(Set[Coord]())((acc, next) => {
//        val edgePoint = direction.scale(next).apply(a)
//
//        if(intersectEdgeMap.getOrElse(edgePoint.y,Set()).contains(edgePoint.x)) {
//          return Set()
//        }
//        acc + edgePoint
//      })
//      edgesPoints
//    }
//    }.toSet
//    //pick a point that CAN'T get to a 0-point
//    val allEdgeAndCoords = edges ++ coords
//    allEdgeAndCoords
//  }
}
