package day14

object day14 {
  val specString = """(\w+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds.""".r
                                                  //> specString  : scala.util.matching.Regex = (\w+) can fly (\d+) km/s for (\d+)
                                                  //|  seconds, but then must rest for (\d+) seconds.
  case class Reindeer(name: String, speed: Int, duration: Int, rest: Int) {
    def distance(timeLeft: Int): Int = {
      if (timeLeft <= 0) 0
      else {
        if (timeLeft <= duration) {
          speed * timeLeft
        } else
          speed * duration + distance(timeLeft - duration - rest)
      }
    }
    def distances(timeLeft: Int, initialDistance: Int): List[Int] = {
      if (timeLeft <= 0) Nil
      else {
        if (timeLeft <= duration) {
          (for {
            i <- 1 to timeLeft
          } yield initialDistance + i * speed).toList
        } else {
          val speeds = for {
            i <- 1 to duration
          } yield initialDistance + i * speed
          val rests = for {
            i <- 1 to rest
            if duration + i <= timeLeft
          } yield initialDistance + duration * speed
          speeds.toList ++ rests.toList ++ distances(timeLeft - duration - rest, initialDistance + duration * speed)
        }
      }
    }
  }

  val reindeer = common.loadPackets(List("day14", "day14.txt")).map {
    case specString(name, speed, duration, rest) => Reindeer(name, speed.toInt, duration.toInt, rest.toInt)
  }                                               //> reindeer  : List[day14.day14.Reindeer] = List(Reindeer(Rudolph,22,8,165), R
                                                  //| eindeer(Cupid,8,17,114), Reindeer(Prancer,18,6,103), Reindeer(Donner,25,6,1
                                                  //| 45), Reindeer(Dasher,11,12,125), Reindeer(Comet,21,6,121), Reindeer(Blitzen
                                                  //| ,18,3,50), Reindeer(Vixen,20,4,75), Reindeer(Dancer,7,20,119))
  reindeer.map(r => r.distance(2503)).max         //> res0: Int = 2696
  
  reindeer.map(r => r.distances(2503, 0))
    .transpose
    .map({ l => l.indexOf(l.max) })
    .groupBy(x => x)
    .map(t => t._2.length)
    .max                                          //> res1: Int = 1084
}