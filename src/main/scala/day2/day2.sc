package day2

object day2 {
  val packets = common.loadPackets(List("day2", "day2.txt")).map { _.split('x').toList map { _.toInt } }
                                                  //> packets  : List[List[Int]] = List(List(4, 23, 21), List(22, 29, 19), List(11
                                                  //| , 4, 11), List(8, 10, 5), List(24, 18, 16), List(11, 25, 22), List(2, 13, 20
                                                  //| ), List(24, 15, 14), List(14, 22, 2), List(30, 7, 3), List(30, 22, 25), List
                                                  //| (29, 9, 9), List(29, 29, 26), List(14, 3, 16), List(1, 10, 26), List(29, 2, 
                                                  //| 30), List(30, 10, 25), List(10, 26, 20), List(1, 2, 18), List(25, 18, 5), Li
                                                  //| st(21, 3, 24), List(2, 5, 7), List(22, 11, 21), List(11, 8, 8), List(16, 18,
                                                  //|  2), List(13, 3, 8), List(1, 16, 19), List(19, 16, 12), List(21, 15, 1), Lis
                                                  //| t(29, 9, 4), List(27, 10, 8), List(2, 7, 27), List(2, 20, 23), List(24, 11, 
                                                  //| 5), List(2, 8, 27), List(10, 28, 10), List(24, 11, 10), List(19, 2, 12), Lis
                                                  //| t(27, 5, 10), List(1, 14, 25), List(5, 14, 30), List(15, 26, 12), List(23, 2
                                                  //| 0, 22), List(5, 12, 1), List(9, 26, 9), List(23, 25, 5), List(28, 16, 19), L
                                                  //| ist(17, 23, 17), List(2, 27, 20), List(18, 27, 13), List(16, 7, 18), List(22
                                                  //| , 7, 29), List(17, 28, 6
                                                  //| Output exceeds cutoff limit.

  def remove[T](list: List[T], index: Int) = {
    val (start, _ :: end) = list.splitAt(index)
    start ::: end
  }                                               //> remove: [T](list: List[T], index: Int)List[T]

  packets map { dim =>
    val sides = for {
      index <- 0 until dim.length;
      val l :: b :: Nil = remove(dim, index)
    } yield l * b
    2 * sides.sum + sides.min
  } sum                                           //> res0: Int = 1598415

  packets map { dim =>
    val smallest = dim.sorted take 2
    2 * smallest.sum + dim.product
  } sum                                           //> res1: Int = 3812909
}