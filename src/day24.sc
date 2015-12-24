object day24 {
  val input = common.loadPackets(List("day24.txt")).map(_.toLong).reverse.toSet

  input.sum / 4

  val comp1 = input.toList.combinations(4).toList.filter(_.sum == 390).map(_.toSet).distinct
  val x:List[Set[Long]] = comp1.toList
  x.map(_.toList.product).sorted.head
}