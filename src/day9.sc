object day9 {
  def lines = common.loadPackets(List("day9", "day9.txt"))
  type City = String
  type Roads = Map[Set[City], Int]

  val pattern = """(\w+) to (\w+) = ([0-9]+)""".r

  def parse(line: String): (Set[City], Int) = line match { case pattern(from, to, distance) => (Set(from, to), distance.toInt) }

  val emptyMap: Roads = Map()
  val roads = lines.map(parse).toMap
  val cities = roads.keySet.reduceLeft(_ union _)

  /**
   * Creates all route past toVisit starting in city
   */
  def visit(city: City, toVisit: Set[City]): Set[Int] = {
    if (toVisit.isEmpty) { Set(0) }
    else for {
      nextCity <- toVisit if roads.contains(Set(city, nextCity))
      restDistance <- visit(nextCity, toVisit - nextCity)
    } yield roads.get(Set(city, nextCity)).get + restDistance
  }
  (cities flatMap { city => visit(city, cities - city) }) min
}