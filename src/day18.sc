object day18 {
  type Board = Vector[Vector[Boolean]]
  val input: Board = common.loadPackets(List("day18.txt")).map { line => line.map({ case '#' => true; case _ => false }).toVector }.toVector

  def lineToString(line: Vector[Boolean]): String = {
    line.map({ x: Boolean => if (x) '#' else '.' }).mkString
  }

  def print(lines: Board): Unit = {
    println(lines.map(lineToString).mkString("\n"))
  }

  def lookup(board: Board, i: Int, j: Int): Boolean =
    try {
      board(i)(j)
    } catch {
      case e: Exception => false
    }

  def numberOfLiveNeighbors(rows: Board, i: Int, j: Int) = {
    (for {
      ii <- (i - 1) to (i + 1)
      jj <- (j - 1) to (j + 1)
      if lookup(rows, ii, jj) && ((ii, jj) !=(i, j))
    } yield 1).sum
  }

  def isCorner(i: Int, j: Int, size: Int): Boolean = {
    ((i == 0) && (j == 0)) ||
      ((i == size - 1) && (j == 0)) ||
      ((i == 0) && (j == size - 1)) ||
      ((i == size - 1) && (j == size - 1))
  }

  def nextState(rows: Vector[Vector[Boolean]], i: Int, j: Int): Boolean = {
    isCorner(i, j, rows.size) || {
      (numberOfLiveNeighbors(rows, i, j), rows(i)(j)) match {
        case (3, _) => true
        case (2, true) => true
        case _ => false
      }
    }
  }

  def nextGeneration(rows: Board, i: Int): Board = {
    (for (i <- rows.indices) yield {
        (for (j <- rows(0).indices) yield nextState(rows, i, j)).toVector
      }).toVector
  }

  def countLights(rows: Board): Int = {
    rows.map(x => x.count(y => y)).sum
  }

  val result = (1 to 100).foldLeft(input)(nextGeneration)
  countLights(result)
}