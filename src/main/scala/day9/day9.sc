package day9

object day9 {
  def lines = common.loadPackets(List("day9", "day9.txt"))
                                                  //> lines: => List[String]

  type City = String
  type Roads = Map[Set[City], Int]

  val pattern = """(\w+) to (\w+) = ([0-9]+)""".r //> pattern  : scala.util.matching.Regex = (\w+) to (\w+) = ([0-9]+)

  def parse(line: String): (Set[City], Int) = line match { case pattern(from, to, distance) => (Set(from, to), distance.toInt) }
                                                  //> parse: (line: String)(Set[day9.day9.City], Int)
  val emptyMap: Roads = Map()                     //> emptyMap  : day9.day9.Roads = Map()
  val roads = lines.map(parse).foldLeft(emptyMap)(
    (roads: Roads, road: (Set[City], Int)) =>
      road match {
        case (cities, distance) =>
          roads.updated(cities, distance)
      })                                          //> roads  : day9.day9.Roads = Map(Set(Faerun, Tambi) -> 71, Set(Norrath, Tambi)
                                                  //|  -> 82, Set(Faerun, Snowdin) -> 60, Set(Tambi, Straylight) -> 70, Set(Faerun
                                                  //| , Arbre) -> 24, Set(Tristram, Arbre) -> 122, Set(Snowdin, Straylight) -> 99,
                                                  //|  Set(Faerun, Straylight) -> 67, Set(Norrath, Arbre) -> 135, Set(AlphaCentaur
                                                  //| i, Tambi) -> 18, Set(Arbre, Straylight) -> 40, Set(Faerun, Norrath) -> 129, 
                                                  //| Set(AlphaCentauri, Arbre) -> 116, Set(Faerun, Tristram) -> 58, Set(Tristram,
                                                  //|  Straylight) -> 97, Set(Arbre, Tambi) -> 53, Set(Norrath, Tristram) -> 142, 
                                                  //| Set(AlphaCentauri, Straylight) -> 91, Set(AlphaCentauri, Snowdin) -> 12, Set
                                                  //| (Tristram, AlphaCentauri) -> 118, Set(Snowdin, Tambi) -> 15, Set(Arbre, Snow
                                                  //| din) -> 129, Set(Norrath, Snowdin) -> 75, Set(Tristram, Tambi) -> 49, Set(Tr
                                                  //| istram, Snowdin) -> 103, Set(Norrath, Straylight) -> 54, Set(Faerun, AlphaCe
                                                  //| ntauri) -> 13, Set(Norrath, AlphaCentauri) -> 15)

  val cities = roads.keySet.reduceLeft(_ union _) //> cities  : Set[day9.day9.City] = Set(Faerun, Snowdin, AlphaCentauri, Tristram
                                                  //| , Straylight, Arbre, Norrath, Tambi)

  /**
   * Creates a route past a set of cities starting in city
   */
  def visit(city: City, toVisit: Set[City]): Set[Int] = {
    if (toVisit.isEmpty) { Set(0) }
    else for {
      nextCity <- toVisit
      restDistance <- visit(nextCity, toVisit - nextCity)
      stepDistance <- roads.get(Set(city, nextCity))
    } yield stepDistance + restDistance
  }                                               //> visit: (city: day9.day9.City, toVisit: Set[day9.day9.City])Set[Int]
  (cities flatMap { city => visit(city, cities - city) }) min
                                                  //> res0: Int = 207
}