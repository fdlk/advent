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
  def nextState(rows: Vector[Vector[Boolean]], i: Int, j: Int): Boolean = {
    if ((i == 0) && (j == 0)) true
    else if ((i == rows.size-1) && (j == 0)) true
    else if ((i == 0) && (j == rows.size-1)) true
    else if ((i == rows.size-1) && (j == rows.size-1)) true
    else {
      val currentlyOn = rows(i)(j)
      val neighbors = numberOfLiveNeighbors(rows, i, j)
      (neighbors, currentlyOn) match {
        case (3, _) => true
        case (2, true) => true
        case _ => false
      }
    }
  }

  def nextGeneration(rows: Board, i: Int): Board = {
    println(i)
    print(rows)
    (for (i <- rows.indices)
      yield {
        (for (j <- rows(0).indices)
          yield nextState(rows, i, j)
          ).toVector
      }).toVector
  }

  def countLights(rows: Board): Int = {
    rows.map(x => x.count(y => y)).sum
  }

  val result = (1 to 100).foldLeft(input)(nextGeneration)
  countLights(result)
}