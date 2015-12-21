import scala.util.parsing.json.JSON

object day12 {
  val input:String = common.loadPackets(List("day12", "day12.txt"))(0)
  val numbers = """-?[0-9]+""".r
  numbers.findAllMatchIn(input).map { _.toString.toInt }.sum

  def sum(tree: Any): Int = tree match {
    case map: Map[String, Any] if map.values.exists { _ == "red" } => 0
    case map: Map[String, Any] => {
      map.map({ case (k: String, v: Any) => sum(k) + sum(v) }).sum
    }
    case list: List[Any] => {
      list.map(sum).sum
    }
    case s: String => 0
    case d: Double => d.toInt
  }

  sum(JSON.parseFull(input).get)
}