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

  def count(string: String, substring: String): Int = {
    substring.r.findAllMatchIn(string).length
  }
  mol.count(_.isUpper) - count(mol, "Rn") - count(mol, "Ar") - 2 * count(mol, "Y") - 1
}