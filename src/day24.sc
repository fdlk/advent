object day24 {
  val input = common.loadPackets(List("day24.txt")).map(_.toLong).reverse

  input.combinations(6).toList.filter(_.sum == input.sum / 3).distinct
    .map(_.product).sorted.head

  input.combinations(4).toList.filter(_.sum == input.sum / 4).distinct
    .map(_.product).sorted.head



}