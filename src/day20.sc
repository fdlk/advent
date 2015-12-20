import scala.annotation.tailrec
object day20 {
  val visited: scala.collection.mutable.Map[Int, Int] = scala.collection.mutable.Map.empty[Int, Int]
  def elves(limit: Int) = {
    @tailrec
    def elf(candidate: Int): Unit = {
      if (candidate <= limit) {
        for {i <- (candidate to limit by candidate) take 50} {
          visited.put(i, visited.get(i).getOrElse(0) + candidate)
        }
        elf(candidate + 1)
      }
    }
    elf(1)
  }
  val (max, presents) = (1000000, 34000000 / 11 + 1)
  elves(max)
  (1 to max).find {visited.getOrElse(_, 0) >= presents}
}