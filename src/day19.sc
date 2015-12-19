import scala.util.Random
object day19 {
  val mol: String = "CRnCaSiRnBSiRnFArTiBPTiTiBFArPBCaSiThSiRnTiBPBPMgArCaSiRnTiMgArCaSiThCaSiRnFArRnSiRnFArTiTiBFArCaCaSiRnSiThCaCaSiRnMgArFYSiRnFYCaFArSiThCaSiThPBPTiMgArCaPRnSiAlArPBCaCaSiRnFYSiThCaRnFArArCaCaSiRnPBSiRnFArMgYCaCaCaCaSiThCaCaSiAlArCaCaSiRnPBSiAlArBCaCaCaCaSiThCaPBSiThPBPBCaSiRnFYFArSiThCaSiRnFArBCaCaSiRnFYFArSiThCaPBSiThCaSiRnPMgArRnFArPTiBCaPRnFArCaCaCaCaSiRnCaCaSiRnFYFArFArBCaSiThFArThSiThSiRnTiRnPMgArFArCaSiThCaPBCaSiRnBFArCaCaPRnCaCaPMgArSiRnFYFArCaSiThRnPBPMgAr"
  val input = common.loadPackets(List("day19.txt"))
  def parseLine(line: String): (String, String) = {
    val x = line.split(" => ")
    (x(0), x(1))
  }
  val tokens = input.map(parseLine)

  //.sortBy({ case (k: String, v: String) => -v.length })
  def replacements(source: String, prefix: String): Set[String] = {
    source match {
      case "" => Set()
      case _ => {
        val char0 = source.substring(0, 1)
        val replaceSomethingElse = replacements(source.substring(1), prefix + char0)
        val replaceThis = (for {
          (k, v) <- tokens if source.startsWith(k)
        } yield prefix + v + source.substring(k.length)).toSet
        replaceSomethingElse ++ replaceThis
      }
    }
  }
  replacements(mol, "").size
  def parse(molString: String, replacements: Int): Option[Int] = {
    if (molString == "e") {
      println(replacements)
      Some(replacements)
    } else {
      val tokenFits: List[(String, String)] =
        tokens.filter({ case (_, v: String) => molString.contains(v) })
      val options: List[Int] = for {
        (k, v) <- Random.shuffle(tokenFits)
        repls <- parse(molString.replaceFirst(v, k), replacements + 1)
      } yield repls
      if (options.isEmpty) {
        None
      } else Some(options.min)
    }
  }
  val result = parse(mol, 0)
  result
}