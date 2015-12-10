package day8

object day8 {
  val dictionaryPath = List("day8", "day8.txt")   //> dictionaryPath  : List[String] = List(day8, day8.txt)

  def lines = {
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
  }                                               //> lines: => List[String]
  
  def memSizeInternal(s: String): Int = {
  	val s2 = s.replaceAll("\\\\\\\\", "B")
  	val s3 = "\\\\\"".r.replaceAllIn(s2,"Q")
  	val s4 = "\\\\x[0-9a-f]{1,2}".r.replaceAllIn(s3, "X")
		//print((s,s2,s3,s4,s4.length))
  	s4.length
  }                                               //> memSizeInternal: (s: String)Int
  
  def memSizeExpanded(s: String): Int = {
  	s.length + (s.filter(c => """\"""".contains(c)).size) + 2
  }                                               //> memSizeExpanded: (s: String)Int
    
  def memSize(s: String): Int = memSizeInternal(s.substring(1, s.length-1))
                                                  //> memSize: (s: String)Int
  lines map {x=>(memSizeExpanded(x) - x.length)} sum
                                                  //> res0: Int = 2074
}