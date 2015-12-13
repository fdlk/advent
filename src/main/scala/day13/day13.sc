package day13

import common._
import scala.collection.mutable.MultiMap

object day13 {

  type Guest = String

  val gain = """(\w+) would gain ([0-9]+) happiness units by sitting next to (\w+).""".r
                                                  //> gain  : scala.util.matching.Regex = (\w+) would gain ([0-9]+) happiness unit
                                                  //| s by sitting next to (\w+).
  val lose = """(\w+) would lose ([0-9]+) happiness units by sitting next to (\w+).""".r
                                                  //> lose  : scala.util.matching.Regex = (\w+) would lose ([0-9]+) happiness unit
                                                  //| s by sitting next to (\w+).

  def parse(line: String) = line match {
    case gain(guest, difference, nextTo) => (Set(guest, nextTo) -> difference.toInt)
    case lose(guest, difference, nextTo) => (Set(guest, nextTo) -> -difference.toInt)
  }                                               //> parse: (line: String)(scala.collection.immutable.Set[String], Int)

  def combineRules(rules:Map[Set[Guest],Int], rule:(Set[Guest], Int)) = {
    rule match {
      case (guests, diff) => rules.updated(guests, rules.getOrElse(guests, 0) + diff)
    }
  }                                               //> combineRules: (rules: Map[Set[day13.day13.Guest],Int], rule: (Set[day13.day1
                                                  //| 3.Guest], Int))scala.collection.immutable.Map[Set[day13.day13.Guest],Int]

  val emptyMap: Map[Set[Guest], Int] = Map()      //> emptyMap  : Map[Set[day13.day13.Guest],Int] = Map()
  val rules: Map[Set[Guest], Int] = common.loadPackets(List("day13", "day13.txt")).map(parse).foldLeft(emptyMap)(combineRules)
                                                  //> rules  : Map[Set[day13.day13.Guest],Int] = Map(Set(Eric, Carol) -> 195, Set(
                                                  //| Mallory, Bob) -> -9, Set(Bob, Alice) -> 42, Set(Mallory, Alice) -> 131, Set(
                                                  //| Eric, Alice) -> -140, Set(Frank, Eric) -> 118, Set(George, Eric) -> -37, Set
                                                  //| (Mallory, Carol) -> 43, Set(Eric, Bob) -> 87, Set(Frank, David) -> -88, Set(
                                                  //| David, Alice) -> -46, Set(Carol, Alice) -> -9, Set(Frank, Bob) -> 132, Set(G
                                                  //| eorge, David) -> 94, Set(Mallory, David) -> -39, Set(George, Bob) -> 5, Set(
                                                  //| George, Alice) -> -6, Set(David, Carol) -> -116, Set(Eric, David) -> 33, Set
                                                  //| (George, Frank) -> 9, Set(David, Bob) -> -33, Set(Mallory, Frank) -> -165, S
                                                  //| et(Frank, Alice) -> -6, Set(Carol, Bob) -> -160, Set(Frank, Carol) -> 98, Se
                                                  //| t(Mallory, George) -> 8, Set(George, Carol) -> 1, Set(Mallory, Eric) -> -13)
                                                  //| 

  val guests: Set[Guest] = rules.keySet.reduceLeft(_ union _)
                                                  //> guests  : Set[day13.day13.Guest] = Set(Carol, George, Bob, Frank, Eric, Alic
                                                  //| e, David, Mallory)

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
    (for {
      pair <- circle.sliding(2).map { _.toSet }
      happiness <- rules.get(pair)
    } yield happiness).sum
  }                                               //> happiness: (combinations: List[day13.day13.Guest])Int

  val sol1 = combinations(guests) map happiness max
                                                  //> sol1  : Int = 733
  val sol2 = combinations(guests + "Fleur") map happiness max
                                                  //> sol2  : Int = 725
}