import scala.annotation.tailrec
object day20 {
  val visited: scala.collection.mutable.Map[Int, Int] = scala.collection.mutable.Map.empty[Int, Int]
  def elves(limit: Int) = {
    @tailrec
    def elf(num: Int): Unit = {
      if (num <= limit) {
        for {i <- (num to limit by num) take 50} {
          visited.put(i, visited.get(i).getOrElse(0) + num)
        }
        elf(num + 1)
      }
    }
    elf(1)
  }
  val (max, presents) = (1000000, 34000000 / 11 + 1)
  elves(max)
  (1 to max).find {visited.getOrElse(_, 0) >= presents}
}