object day18 {

  case class Board(lights: Vector[Vector[Boolean]]) {
    def print(): Unit = {
      println(lights.map(_.map {
        if (_) '#' else '.'
      }).mkString("\n"))
    }

    def lookup(i: Int, j: Int): Boolean =
      try {
        lights(i)(j)
      } catch {
        case _: IndexOutOfBoundsException => false
      }

    def numberOfLiveNeighbors(i: Int, j: Int): Int = {
      (for {
        ii <- (i - 1) to (i + 1)
        jj <- (j - 1) to (j + 1)
        if (ii, jj) !=(i, j)
      } yield lookup(ii, jj)).count(identity)
    }

    def nextState(i: Int, j: Int): Boolean = {
      isCorner(i, j, lights.size) || {
        (numberOfLiveNeighbors(i, j), lights(i)(j)) match {
          case (3, _) => true
          case (2, true) => true
          case _ => false
        }
      }
    }

    def nextGeneration(gen: Int): Board = {
      Board((for (i <- lights.indices) yield {
        (for (j <- lights(0).indices) yield nextState(i, j)).toVector
      }).toVector)
    }

    def countLights: Int = {
      lights.map(_.count(identity)).sum
    }
  }

  val input = common.loadPackets(List("day18.txt")).map(_.map(_ == '#').toVector).toVector

  def isCorner(i: Int, j: Int, size: Int): Boolean = {
    val edges = Set(0, size-1)
    edges.contains(i) && edges.contains(j)
  }

  (1 to 100).foldLeft(Board(input))(_.nextGeneration(_)).countLights
}