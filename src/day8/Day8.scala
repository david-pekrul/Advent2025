package day8

object Day8 {
  def main(args: Array[String]): Unit = {
    val input = helpers.Helpers.readTestFile(this)

    val points = input.map(line => {
      line.split(",").map(_.toInt).toSeq
    })

    val kdTree = buildKDTree(points)

    val thing = searchKDTree(Seq(425,690,689),kdTree)

    println("here")
  }

  def buildKDTree(points: Seq[Seq[Int]], currentDepth: Int = 0): KDNode = {

    val dimensions = points.head.size
    if(points.size == 1) {
      return KDNode(points.head, EMPTY_NODE, EMPTY_NODE, currentDepth % dimensions)
    }

    val sortedPoints = sortPointsByDimension(points, currentDepth)
    val point = sortedPoints(sortedPoints.size/2)
    val firstHalf = sortedPoints.takeWhile(_ != point)
    val lastHalf = sortedPoints.drop(firstHalf.size + 1)

    val firstHalfNode = firstHalf match {
      case Seq() => EMPTY_NODE
      case x => buildKDTree(x, currentDepth +1)
    }
    val lastHalfNode = lastHalf match {
      case Seq() => EMPTY_NODE
      case x => buildKDTree(x, currentDepth +1)
    }

    KDNode(point, firstHalfNode, lastHalfNode, currentDepth % dimensions)
  }

  private val EMPTY_NODE = KDNode(Seq(Int.MaxValue, Int.MaxValue, Int.MaxValue),null,null, -1)
  case class KDNode(point: Seq[Int],
                    firstHalf: KDNode,
                    secondHalf: KDNode,
                    sortIndex: Int) {
      lazy val isLeaf: Boolean = {
        firstHalf == EMPTY_NODE && secondHalf == EMPTY_NODE
      }

      def distanceToPoint(point: Seq[Int]): Double = {
        distance(this.point, point)
      }

      override def toString: String = {
        "[" + point.mkString(";") + s"f:${firstHalf},s:${secondHalf}]"
      }
  }


  def searchKDTree(target: Seq[Int], kdTree: KDNode): KDNode = {
    //https://medium.com/@isurangawarnasooriya/exploring-kd-trees-a-comprehensive-guide-to-implementation-and-applications-in-python-3385fd56a246
    def _search(nodeOpt: KDNode, currentDepth: Int, currentBestNode: Option[KDNode]): KDNode = {
      if(nodeOpt == EMPTY_NODE) {
        return currentBestNode.get
      }

      val node = nodeOpt
      val searchDimension = currentDepth % target.size

      val nextBest = if (currentBestNode.isEmpty || (distance(target, node.point) < distance(target, currentBestNode.get.point) && node.point != target)) {
        node
      } else {
        currentBestNode.get
      }

      val (nextBranch,otherBranch) = if (target(searchDimension) < node.point(searchDimension)) {
        (node.firstHalf, node.secondHalf)
      } else {
        (node.secondHalf, node.firstHalf)
      }

      val firstSubSearch = _search(nextBranch, currentDepth + 1, Some(nextBest))

      val secondSubSearch = if (distance(target, firstSubSearch.point) > Math.abs(target(searchDimension) - node.point(searchDimension))) {
        _search(otherBranch, currentDepth + 1, Some(firstSubSearch))
      } else {
        firstSubSearch
      }

      secondSubSearch
    }

    val closestNode = _search(kdTree,0, None)
    closestNode
  }

  def sortPointsByDimension(points: Seq[Seq[Int]], sortDimension: Int): Seq[Seq[Int]] = {
    points.sortBy(point => point(sortDimension % point.size))
  }

  def distance(a: Seq[Int], b: Seq[Int]): Double = {
    val x = a.zip(b).map{case (c,d) => d-c}.map(Math.pow(_,2)).sum
    Math.sqrt(x)
  }

  def getPointDimension(point: Seq[Int], currentDepth: Int): Int = {
    point(currentDepth % point.size)
  }

  def getPointDimension(kdNode: KDNode, currentDepth: Int): Int = {
    getPointDimension(kdNode.point, currentDepth)
  }
}
