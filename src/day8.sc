object day8 {
  def lines = common.loadPackets(List("day8", "day8.txt"))

  def memSizeInternal(s: String): Int = {
    val s2 = s.replaceAll("\\\\\\\\", "B")
    val s3 = "\\\\\"".r.replaceAllIn(s2, "Q")
    val s4 = "\\\\x[0-9a-f]{1,2}".r.replaceAllIn(s3, "X")
    s4.length
  }

  def memSizeExpanded(s: String): Int = {
    s.length + s.count(c => """\"""".contains(c)) + 2
  }

  def memSize(s: String): Int = memSizeInternal(s.substring(1, s.length - 1))

  lines map { x => memSizeExpanded(x) - x.length } sum
}