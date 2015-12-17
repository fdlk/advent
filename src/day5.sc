object day5 {
  val input = common.loadPackets( List("day5", "day5.txt"))

  def hasThreeVowels(s: String): Boolean = s.count("aeiou".contains(_)) > 2
  def hasRepeatingLetter(s: String): Boolean = s.toList.sliding(2).exists { case List(a, b) => a == b }
  def hasNoForbiddenSubstring(s: String): Boolean = List("ab", "cd", "pq", "xy").forall { !s.contains(_) }
  def isNice(s: String): Boolean = hasThreeVowels(s) && hasRepeatingLetter(s) && hasNoForbiddenSubstring(s)

  input.count(isNice)

  def nonOverlappingPairs(x:(String, List[(String, Int)])) = {
    val indices = x._2.map { _._2 }
    indices.max - indices.min > 1
  }

  def hasRepeatingPair(s: String) = {
    s.sliding(2).zipWithIndex.toList.groupBy { _._1 }.exists(nonOverlappingPairs)
  }

	def hasRepeatingLetterWithOneInBetween(s: String) = {
		s.sliding(3, 1).exists{x => x.length > 2 && x(0) == x(2)}
	}

  def isNice2(s: String) = hasRepeatingPair(s) && hasRepeatingLetterWithOneInBetween(s)

  input.count(isNice2)

}