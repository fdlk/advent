package day2

object day2 {
  val input = common.loadPackets(List("day2", "day2.txt"))
                                                  //> input  : List[String] = List(4x23x21, 22x29x19, 11x4x11, 8x10x5, 24x18x16, 11
                                                  //| x25x22, 2x13x20, 24x15x14, 14x22x2, 30x7x3, 30x22x25, 29x9x9, 29x29x26, 14x3x
                                                  //| 16, 1x10x26, 29x2x30, 30x10x25, 10x26x20, 1x2x18, 25x18x5, 21x3x24, 2x5x7, 22
                                                  //| x11x21, 11x8x8, 16x18x2, 13x3x8, 1x16x19, 19x16x12, 21x15x1, 29x9x4, 27x10x8,
                                                  //|  2x7x27, 2x20x23, 24x11x5, 2x8x27, 10x28x10, 24x11x10, 19x2x12, 27x5x10, 1x14
                                                  //| x25, 5x14x30, 15x26x12, 23x20x22, 5x12x1, 9x26x9, 23x25x5, 28x16x19, 17x23x17
                                                  //| , 2x27x20, 18x27x13, 16x7x18, 22x7x29, 17x28x6, 9x22x17, 10x5x6, 14x2x12, 25x
                                                  //| 5x6, 26x9x10, 19x21x6, 19x4x27, 23x16x14, 21x17x29, 24x18x10, 7x19x6, 14x15x1
                                                  //| 0, 9x10x19, 20x18x4, 11x14x8, 30x15x9, 25x12x24, 3x12x5, 12x21x28, 8x23x10, 1
                                                  //| 8x26x8, 17x1x8, 2x29x15, 3x13x28, 23x20x11, 27x25x6, 19x21x3, 30x22x27, 28x24
                                                  //| x4, 26x18x21, 11x7x16, 22x27x6, 27x5x26, 4x10x4, 4x2x27, 2x3x26, 26x29x19, 30
                                                  //| x26x24, 8x25x12, 16x17x5, 13x2x3, 1x30x22, 20x9x1, 24x26x19, 26x18x1, 18x29x2
                                                  //| 4, 1x6x9, 20x27x2, 3x22x2
                                                  //| Output exceeds cutoff limit.

  def remove[T](list: List[T], index: Int) = {
    val (start, _ :: end) = list.splitAt(index)
    start ::: end
  }                                               //> remove: [T](list: List[T], index: Int)List[T]

  def paperRequired(dimensions: String) = {
    val dims = dimensions.split('x').toList
    val sides = for {
      index <- 0 until dims.length
    } yield {
      val l :: b :: Nil = remove(dims, index)
      l.toInt * b.toInt
    }
    2 * sides.sum + sides.min
  }                                               //> paperRequired: (dimensions: String)Int
  
	val paperReqs = (input map paperRequired) sum
                                                  //> paperReqs  : Int = 1598415

  def ribbonRequired(dimensions: String) = {
  	val dims = dimensions.split('x').toList map {_.toInt}
  	val smallest = dims.sorted take 2
  	val p = dims.product
  	2 * smallest.sum + p
  }                                               //> ribbonRequired: (dimensions: String)Int
  
  val ribbonReqs = (input map ribbonRequired) sum //> ribbonReqs  : Int = 3812909
  
  
}