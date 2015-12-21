object day2 {
  val packets:List[List[Int]] = common.loadPackets(List("day2", "day2.txt")).map(_.split('x').toList.map(_.toInt))

  def remove[T](list: List[T], index: Int) = {
    val (start, _ :: end) = list.splitAt(index)
    start ::: end
  }

  packets map { dim =>
    val sides = for {
      index <- dim.indices
      l :: b :: Nil = remove(dim, index)
    } yield l * b
    2 * sides.sum + sides.min
  } sum

  packets map { dim =>
    val smallest = dim.sorted take 2
    2 * smallest.sum + dim.product
  } sum
}