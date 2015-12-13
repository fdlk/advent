package day13

import common._

object day13 {

  type Guest = String
  case class Happiness(guests: Set[Guest], difference: Int)

  val rules = common.loadPackets(List("day13", "day13.txt")).map { line =>
    line match {
      case r"""(\w+)$guest would gain ([0-9]+)$difference happiness units by sitting next to (\w+)$nextTo.""" =>
        Happiness(Set(guest, nextTo), difference.toInt)
      case r"""(\w+)$guest would lose ([0-9]+)$difference happiness units by sitting next to (\w+)$nextTo.""" =>
        Happiness(Set(guest, nextTo), -difference.toInt)
    }
  }.toList                                        //> rules  : List[day13.day13.Happiness] = List(Happiness(Set(Alice, Bob),2), Ha
                                                  //| ppiness(Set(Alice, Carol),26), Happiness(Set(Alice, David),-82), Happiness(S
                                                  //| et(Alice, Eric),-75), Happiness(Set(Alice, Frank),42), Happiness(Set(Alice, 
                                                  //| George),38), Happiness(Set(Alice, Mallory),39), Happiness(Set(Bob, Alice),40
                                                  //| ), Happiness(Set(Bob, Carol),-61), Happiness(Set(Bob, David),-15), Happiness
                                                  //| (Set(Bob, Eric),63), Happiness(Set(Bob, Frank),41), Happiness(Set(Bob, Georg
                                                  //| e),30), Happiness(Set(Bob, Mallory),87), Happiness(Set(Carol, Alice),-35), H
                                                  //| appiness(Set(Carol, Bob),-99), Happiness(Set(Carol, David),-51), Happiness(S
                                                  //| et(Carol, Eric),95), Happiness(Set(Carol, Frank),90), Happiness(Set(Carol, G
                                                  //| eorge),-16), Happiness(Set(Carol, Mallory),94), Happiness(Set(David, Alice),
                                                  //| 36), Happiness(Set(David, Bob),-18), Happiness(Set(David, Carol),-65), Happi
                                                  //| ness(Set(David, Eric),-18), Happiness(Set(David, Frank),-22), Happiness(Set(
                                                  //| David, George),2), Happi
                                                  //| Output exceeds cutoff limit.

  val guests = (rules flatMap { _.guests }).toSet //> guests  : scala.collection.immutable.Set[day13.day13.Guest] = Set(Carol, Geo
                                                  //| rge, Bob, Frank, Eric, Alice, David, Mallory)

  def combinations(users: Set[Guest]): Set[List[Guest]] = {
    if (users.isEmpty) Set(Nil) else {
      for {
        user <- users;
        combination <- combinations(users - user)
      } yield user :: combination
    }
  }                                               //> combinations: (users: Set[day13.day13.Guest])Set[List[day13.day13.Guest]]

  def happiness(combinations: List[Guest]): Int = {
    val circle = combinations ::: (combinations take 1)
    val pairs = circle.sliding(2).map { _.toSet }.toList
    val validRules = rules.filter(rule => { pairs.contains(rule.guests) })
    validRules.map { _.difference }.sum
  }                                               //> happiness: (combinations: List[day13.day13.Guest])Int

  val sol1 = combinations(guests) map happiness max
                                                  //> sol1  : Int = 733
  val sol2 = combinations(guests + "Fleur") map happiness max
                                                  //> sol2  : Int = 725
}