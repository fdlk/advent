/**
  * Created by fkelpin on 20/12/15.
  */
object Day20App extends App {

  override def main (args: Array[String]) {
    val (max, numPresents) = (800000, 34000000 / 10)
    val presents = (for {
      num <- 1 to max
      i <- (num to max by num)
    } yield {
      (i, num)
    }).groupBy(_._1).toList.sortBy(_._1)
    println(presents.find(_._2.map(_._2).sum > numPresents).map(_._1))
  }

}
