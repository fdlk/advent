package day2

object day2 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  val dictionaryPath = List("day2", "day2.txt")   //> dictionaryPath  : List[String] = List(day2, day2.txt)

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

  def remove[T](list: List[T], index: Int) = {
    val (start, _ :: end) = list.splitAt(index)
    start ::: end
  }                                               //> remove: [T](list: List[T], index: Int)List[T]

  def paperRequired(dimensions: String) = {
    val dims = dimensions.split('x').toList
    val sides = for {
      index <- 0 until dims.length
    } yield {
      val l :: b :: Nil = remove(dims, index)
      l.toInt * b.toInt
    }
    2 * sides.sum + sides.min
  }                                               //> paperRequired: (dimensions: String)Int
  
	val paperReqs = (loadPackets map paperRequired) sum
                                                  //> paperReqs  : Int = 1598415

  def ribbonRequired(dimensions: String) = {
  	val dims = dimensions.split('x').toList map {_.toInt}
  	val smallest = dims.sorted take 2
  	val p = dims.product
  	2 * smallest.sum + p
  }                                               //> ribbonRequired: (dimensions: String)Int
  
  val ribbonReqs = (loadPackets map ribbonRequired) sum
                                                  //> ribbonReqs  : Int = 3812909
  
  
}