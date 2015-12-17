object day17 {
  val input = common.loadPackets(List("day17", "day17.txt")) map {
    _.toInt
  }
  case class Box(index: Int, capacity: Int)
  val inputAsBoxes = input.zipWithIndex.map { case (c, i) => Box(i, c) }
  Range(0, input.length+1)
    .flatMap(inputAsBoxes.combinations)
    .count({_.map{_.capacity}.sum ==150})
  val boxCountStep2 = Range(0, input.length+1)
    .find(inputAsBoxes.combinations(_).toList.exists({boxes => boxes.map{_.capacity}.sum ==150})).get
  inputAsBoxes.combinations(boxCountStep2).toList.count({boxes => boxes.map{_.capacity}.sum ==150})
}