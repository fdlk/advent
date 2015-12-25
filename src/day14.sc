object day14 {
  val specString = """(\w+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds.""".r

  case class Reindeer(name: String, speed: Int, duration: Int, rest: Int) {
    val period = duration + rest

    def distance(timeLeft: Int) = ((timeLeft / period) * duration + Math.min(timeLeft % period, duration)) * speed

    def distances(timeLeft: Int): List[Int] = (for {i <- 1 to timeLeft} yield distance(i)).toList
  }

  val reindeer = common.loadPackets(List("day14", "day14.txt")).map {
    case specString(name, speed, duration, rest) => Reindeer(name, speed.toInt, duration.toInt, rest.toInt)
  }

  reindeer.map(_.distance(2503)).max
  reindeer.map(_.distances(2503))
    .transpose
    .flatMap({ l => l.indices.filter { i => l(i) == l.max } })
    .groupBy(identity)
    .map(t => t._2.length)
    .max
}