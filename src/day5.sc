object day5 {
  val input = common.loadPackets( List("day5", "day5.txt"))
                                                  //> input  : List[String] = List(zgsnvdmlfuplrubt, vlhagaovgqjmgvwq, ffumlmqwfcsy
                                                  //| qpss, zztdcqzqddaazdjp, eavfzjajkjesnlsb, urrvucyrzzzooxhx, xdwduffwgcptfwad,
                                                  //|  orbryxwrmvkrsxsr, jzfeybjlgqikjcow, mayoqiswqqryvqdi, iiyrkoujhgpgkcvx, egcg
                                                  //| upjkqwfiwsjl, zbgtglaqqolttgng, eytquncjituzzhsx, dtfkgggvqadhqbwb, zettygjpc
                                                  //| oedwyio, rwgwbwzebsnjmtln, esbplxhvzzgawctn, vnvshqgmbotvoine, wflxwmvbhflkqx
                                                  //| vo, twdjikcgtpvlctte, minfkyocskvgubvm, sfxhhdhaopajbzof, sofkjdtalvhgwpql, u
                                                  //| qfpeauqzumccnrc, tdflsbtiiepijanf, dhfespzrhecigzqb, xobfthcuuzhvhzpn, olgjgl
                                                  //| xaotocvrhw, jhkzpfcskutwlwge, zurkakkkpchzxjhq, hekxiofhalvmmkdl, azvxuwwfmjd
                                                  //| pjskj, arsvmfznblsqngvb, ldhkzhejofreaucc, adrphwlkehqkrdmo, wmveqrezfkaivvaw
                                                  //| , iyphmphgntinfezg, blomkvgslfnvspem, cgpaqjvzhbumckwo, ydhqjcuotkeyurpx, sbt
                                                  //| zboxypnmdaefr, vxrkhvglynljgqrg, ttgrkjjrxnxherxd, hinyfrjdiwytetkw, sufltffw
                                                  //| qbugmozk, tohmqlzxxqzinwxr, jbqkhxfokaljgrlg, fvjeprbxyjemyvuq, gmlondgqmlsel
                                                  //| wah, ubpwixgxdloqnvjp, lx
                                                  //| Output exceeds cutoff limit.

  def hasThreeVowels(s: String): Boolean = s.count("aeiou".contains(_)) > 2
                                                  //> hasThreeVowels: (s: String)Boolean
  def hasRepeatingLetter(s: String): Boolean = s.toList.sliding(2).exists { _.toList match { case List(a, b) => a == b } }
                                                  //> hasRepeatingLetter: (s: String)Boolean
  def hasNoForbiddenSubstring(s: String): Boolean = List("ab", "cd", "pq", "xy").forall { !s.contains(_) }
                                                  //> hasNoForbiddenSubstring: (s: String)Boolean
  def isNice(s: String): Boolean = hasThreeVowels(s) && hasRepeatingLetter(s) && hasNoForbiddenSubstring(s)
                                                  //> isNice: (s: String)Boolean

  input.count(isNice)                     //> res0: Int = 238

  def nonOverlappingPairs(x:(String, List[(String, Int)])) = {
    val indices = x._2.map { _._2 }
    indices.max - indices.min > 1
  }                                               //> nonOverlappingPairs: (x: (String, List[(String, Int)]))Boolean

  def hasRepeatingPair(s: String) = {
    s.sliding(2).zipWithIndex.toList.groupBy { _._1 }.exists(nonOverlappingPairs)
  }                                               //> hasRepeatingPair: (s: String)Boolean

	def hasRepeatingLetterWithOneInBetween(s: String) = {
		s.sliding(3, 1).exists{x => x.length > 2 && x(0) == x(2)}
	}                                         //> hasRepeatingLetterWithOneInBetween: (s: String)Boolean
	
	def isNice2(s: String) = hasRepeatingPair(s) && hasRepeatingLetterWithOneInBetween(s)
                                                  //> isNice2: (s: String)Boolean
 
	isNice2("ieodomkazucvgmuy")               //> res1: Boolean = false
  input.count(isNice2)                      //> res2: Int = 69

}