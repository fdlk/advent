package day5

object day5 {
  val dictionaryPath = List("day5", "day5.txt")   //> dictionaryPath  : List[String] = List(day5, day5.txt)

  def loadPackets = {
    val wordstream = Option {
      getClass.getClassLoader.getResourceAsStream(dictionaryPath.mkString("/"))
    } orElse {
      common.resourceAsStreamFromSrc(dictionaryPath)
    } getOrElse {
      sys.error("Could not load word list, dictionary file not found")
    }
    try {
      val s = io.Source.fromInputStream(wordstream)
      s.getLines.toList
    } catch {
      case e: Exception =>
        println("Could not load word list: " + e)
        throw e
    } finally {
      wordstream.close()
    }
  }                                               //> loadPackets: => List[String]

  loadPackets.size                                //> res0: Int = 1000

  def hasThreeVowels(s: String): Boolean = s.filter("aeiou".contains(_)).size > 2
                                                  //> hasThreeVowels: (s: String)Boolean
  def hasRepeatingLetter(s: String): Boolean = s.toList.sliding(2).exists { _.toList match { case List(a, b) => a == b } }
                                                  //> hasRepeatingLetter: (s: String)Boolean
  def hasNoForbiddenSubstring(s: String): Boolean = List("ab", "cd", "pq", "xy").forall { !s.contains(_) }
                                                  //> hasNoForbiddenSubstring: (s: String)Boolean
  def isNice(s: String): Boolean = hasThreeVowels(s) && hasRepeatingLetter(s) && hasNoForbiddenSubstring(s)
                                                  //> isNice: (s: String)Boolean

  loadPackets.filter(isNice).size                 //> res1: Int = 238

  def nonOverlappingPairs(x:(String, List[(String, Int)])) = {
    val indices = x._2.map { _._2 };
    indices.max - indices.min > 1
  }                                               //> nonOverlappingPairs: (x: (String, List[(String, Int)]))Boolean

  def hasRepeatingPair(s: String) = {
    s.sliding(2).zipWithIndex.toList.groupBy { _._1 }.filter(nonOverlappingPairs).nonEmpty
  }                                               //> hasRepeatingPair: (s: String)Boolean

	def hasRepeatingLetterWithOneInBetween(s: String) = {
		s.sliding(3, 1).exists{x => x.length > 2 && x(0) == x(2)}
	}                                         //> hasRepeatingLetterWithOneInBetween: (s: String)Boolean
	
	def isNice2(s: String) = hasRepeatingPair(s) && hasRepeatingLetterWithOneInBetween(s)
                                                  //> isNice2: (s: String)Boolean
 
	isNice2("ieodomkazucvgmuy")               //> res2: Boolean = false
  loadPackets.filter(isNice2).size                //> res3: Int = 69

}