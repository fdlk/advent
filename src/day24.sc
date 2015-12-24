object day24 {
  val input = common.loadPackets(List("day24.txt")).map(_.toLong).reverse
  val solution1 = input.combinations(6).toList.filter(_.sum == input.sum / 3).distinct
    .sortBy(_.product).find(
      candidate => divide(input.toSet -- candidate.toSet, 2, input.sum/3, 6).isDefined
  ).map(_.product)

  val solution2 = input.combinations(4).toList.filter(_.sum == input.sum / 4).distinct
    .sortBy(_.product).find(
    candidate => divide(input.toSet -- candidate.toSet, 3, input.sum/4, 4).isDefined
  ).map(_.product)
  /**
    * Divide items into parts groups, each with a sum of size Long
    */
  def divide (items: Set[Long], parts: Int, sum: Long, minPartitionSize: Int): Option[List[Set[Long]]] = {
    if (parts == 1){
      if(items.sum == sum) {
        Some(List(items))
      } else None
    } else {
      (for {
        partitionSize <- minPartitionSize to items.size
        combination <- items.toList.sortBy(x => -x).combinations(partitionSize)
        if combination.sum == sum
        division:List[Set[Long]] <- divide(items.toSet -- combination.toSet, parts - 1, sum, partitionSize)
      } yield combination.toSet :: division).headOption
    }
  }
}