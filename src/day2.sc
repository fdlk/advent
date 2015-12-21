object day2 {
  val packets: List[List[Int]] = common.loadPackets(List("day2", "day2.txt")).map(_.split('x').toList.map(_.toInt))

  packets map { packet =>
    val sides = (for {
      dims <- packet.indices.combinations(2)
      dim1::dim2::Nil = dims.toList
    } yield packet(dim1) * packet(dim2)).toList
    2 * sides.sum + sides.min
  } sum

  packets map { dim =>
    val smallest = dim.sorted take 2
    2 * smallest.sum + dim.product
  } sum
}