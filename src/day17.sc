object day17 {
  val input = common.loadPackets(List("day17", "day17.txt")) map {
    _.toInt
  }

  Range(0, input.length + 1)
    .flatMap(input
      .zipWithIndex
      .combinations)
    .count(_.map(_._1).sum == 150)

  Range(0, input.length + 1)
    .map(input
      .zipWithIndex
      .combinations(_)
      .count(_.map(_._1).sum == 150))
    .find(_ > 0)
}