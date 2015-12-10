package day9

object day9 {
  val dictionaryPath = List("day9", "day9.txt")   //> dictionaryPath  : List[String] = List(day9, day9.txt)

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

	type City = String
  case class Road(from: City, to: City, distance: Int)

  val pattern = """(\w+) to (\w+) = ([0-9]+)""".r //> pattern  : scala.util.matching.Regex = (\w+) to (\w+) = ([0-9]+)
  val roads = lines flatMap { _ match { case pattern(from, to, distance) => List(Road(from, to, distance.toInt), Road(to, from, distance.toInt)) } }
                                                  //> roads  : List[day9.day9.Road] = List(Road(Faerun,Norrath,129), Road(Norrath,
                                                  //| Faerun,129), Road(Faerun,Tristram,58), Road(Tristram,Faerun,58), Road(Faerun
                                                  //| ,AlphaCentauri,13), Road(AlphaCentauri,Faerun,13), Road(Faerun,Arbre,24), Ro
                                                  //| ad(Arbre,Faerun,24), Road(Faerun,Snowdin,60), Road(Snowdin,Faerun,60), Road(
                                                  //| Faerun,Tambi,71), Road(Tambi,Faerun,71), Road(Faerun,Straylight,67), Road(St
                                                  //| raylight,Faerun,67), Road(Norrath,Tristram,142), Road(Tristram,Norrath,142),
                                                  //|  Road(Norrath,AlphaCentauri,15), Road(AlphaCentauri,Norrath,15), Road(Norrat
                                                  //| h,Arbre,135), Road(Arbre,Norrath,135), Road(Norrath,Snowdin,75), Road(Snowdi
                                                  //| n,Norrath,75), Road(Norrath,Tambi,82), Road(Tambi,Norrath,82), Road(Norrath,
                                                  //| Straylight,54), Road(Straylight,Norrath,54), Road(Tristram,AlphaCentauri,118
                                                  //| ), Road(AlphaCentauri,Tristram,118), Road(Tristram,Arbre,122), Road(Arbre,Tr
                                                  //| istram,122), Road(Tristram,Snowdin,103), Road(Snowdin,Tristram,103), Road(Tr
                                                  //| istram,Tambi,49), Road(T
                                                  //| Output exceeds cutoff limit.
  val cities = (roads flatMap { road:Road => road match { case Road(from, to, _) => List(from, to) }}).toSet
                                                  //> cities  : scala.collection.immutable.Set[String] = Set(Faerun, Snowdin, Alph
                                                  //| aCentauri, Tristram, Straylight, Arbre, Norrath, Tambi)

	def distance (roads: List[Road]) = (roads map {_.distance}) sum
                                                  //> distance: (roads: List[day9.day9.Road])Int

	/**
	 * Creates a route past a set of cities starting in city
	 */
	def visit(city: City, toVisit: Set[City]): Set[List[Road]] = toVisit match {
		case s if s.isEmpty => Set(Nil)
		case _ => for {
			nextCity <- toVisit
			road <- roads if road.from == city && road.to == nextCity
			route <- visit(road.to, toVisit - nextCity)
		} yield road :: route
	}                                         //> visit: (city: day9.day9.City, toVisit: Set[day9.day9.City])Set[List[day9.da
                                                  //| y9.Road]]
	val shortest = cities flatMap {city => visit(city, cities - city)} maxBy(distance)
                                                  //> shortest  : List[day9.day9.Road] = List(Road(Straylight,Snowdin,99), Road(S
                                                  //| nowdin,Arbre,129), Road(Arbre,AlphaCentauri,116), Road(AlphaCentauri,Tristr
                                                  //| am,118), Road(Tristram,Norrath,142), Road(Norrath,Faerun,129), Road(Faerun,
                                                  //| Tambi,71))
	 
	distance(shortest)                        //> res0: Int = 804
}