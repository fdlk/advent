object day24 {
  val input = common.loadPackets(List("day24.txt")).map(_.toLong).reverse.toSet

  input.toList.combinations(6).toList.filter(_.sum == input.sum / 3).map(_.toSet).distinct
  .map(_.toList.product).sorted.head

  input.toList.combinations(4).toList.filter(_.sum == input.sum / 4).map(_.toSet).distinct
  .map(_.toList.product).sorted.head
}