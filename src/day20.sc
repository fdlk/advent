import scala.annotation.tailrec

object day20 {
  val presents: scala.collection.mutable.Map[Int, Int] = scala.collection.mutable.Map.empty[Int, Int]
  val (max, numPresents) = (1000000, 34000000 / 11)

  for (num <- 1 to max;
       i <- (num to max by num) take 50) {
    presents.put(i, presents.get(i).getOrElse(0) + num)
  }
  (1 to max).find {
    presents.getOrElse(_, 0) > numPresents
  }
}