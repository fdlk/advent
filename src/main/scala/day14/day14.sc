package day14

object day14 {
  val specString = """(\w+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds.""".r
                                                  //> specString  : scala.util.matching.Regex = (\w+) can fly (\d+) km/s for (\d+)
                                                  //|  seconds, but then must rest for (\d+) seconds.
  case class Reindeer(name: String, speed: Int, duration: Int, rest: Int) {
    val period = duration + rest
    def distance(timeLeft: Int) = ((timeLeft / period) * duration + Math.min(timeLeft % period, duration)) * speed
    def distances(timeLeft: Int): List[Int] = (for { i <- 1 to timeLeft } yield distance(i)).toList
  }

  val reindeer = common.loadPackets(List("day14", "day14.txt")).map {
    case specString(name, speed, duration, rest) => Reindeer(name, speed.toInt, duration.toInt, rest.toInt)
  }                                               //> reindeer  : List[day14.day14.Reindeer] = List(Reindeer(Rudolph,22,8,165), Re
                                                  //| indeer(Cupid,8,17,114), Reindeer(Prancer,18,6,103), Reindeer(Donner,25,6,145
                                                  //| ), Reindeer(Dasher,11,12,125), Reindeer(Comet,21,6,121), Reindeer(Blitzen,18
                                                  //| ,3,50), Reindeer(Vixen,20,4,75), Reindeer(Dancer,7,20,119))
  reindeer.map(r => r.distance(2503)).max         //> res0: Int = 2696
  reindeer.map(r => r.distances(2503))
    .transpose
    .flatMap({ l => l.indices.filter {i => l(i) == l.max} })
    .groupBy(x => x)
    .map(t => t._2.length)
    .max                                          //> res1: Int = 1084
}