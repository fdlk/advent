package day16

import scala.util.parsing.combinator.JavaTokenParsers

object day16 {

  case class Aunt(sue: String, props: Map[String, Int])

  class AuntParser extends JavaTokenParsers {
    def thing: Parser[String] = "children" | "cats" | "samoyeds" | "akitas" | "vizslas" | "goldfish" | "trees" | "cars" | "perfumes" | "pomeranians" ^^ { x => x }
    def aunt: Parser[Aunt] = "Sue " ~> (wholeNumber) ~ ": " ~ repsep(property, ",") ^^ { case sue ~ ": " ~ props => Aunt(sue, props.toMap) }
    def property: Parser[(String, Int)] = thing ~ ": " ~ wholeNumber ^^ { case name ~ ": " ~ value => (name, value.toInt) }
  }

  object AuntParser extends AuntParser

  val aunts = common.loadPackets(List("day16", "day16.txt")) map { AuntParser.parseAll(AuntParser.aunt, _).get }
                                                  //> aunts  : List[day16.day16.Aunt] = List(Aunt(1,Map(goldfish -> 6, trees -> 9,
                                                  //|  akitas -> 0)), Aunt(2,Map(goldfish -> 7, trees -> 1, akitas -> 0)), Aunt(3,
                                                  //| Map(cars -> 10, akitas -> 6, perfumes -> 7)), Aunt(4,Map(perfumes -> 2, vizs
                                                  //| las -> 0, cars -> 6)), Aunt(5,Map(goldfish -> 1, trees -> 3, perfumes -> 10)
                                                  //| ), Aunt(6,Map(children -> 9, vizslas -> 7, cars -> 9)), Aunt(7,Map(cars -> 6
                                                  //| , vizslas -> 5, cats -> 3)), Aunt(8,Map(akitas -> 10, vizslas -> 9, children
                                                  //|  -> 3)), Aunt(9,Map(vizslas -> 8, cats -> 2, trees -> 1)), Aunt(10,Map(perfu
                                                  //| mes -> 10, trees -> 6, cars -> 4)), Aunt(11,Map(cars -> 9, children -> 1, ca
                                                  //| ts -> 1)), Aunt(12,Map(pomeranians -> 4, akitas -> 6, goldfish -> 8)), Aunt(
                                                  //| 13,Map(cats -> 10, children -> 5, trees -> 9)), Aunt(14,Map(perfumes -> 8, v
                                                  //| izslas -> 3, samoyeds -> 1)), Aunt(15,Map(vizslas -> 2, perfumes -> 8, trees
                                                  //|  -> 3)), Aunt(16,Map(pomeranians -> 10, trees -> 9, samoyeds -> 4)), Aunt(17
                                                  //| ,Map(akitas -> 7, vizsla
                                                  //| Output exceeds cutoff limit.
  val theRealSue = Aunt("theRealSue",
    Map("children" -> 3, "cats" -> 7, "samoyeds" -> 2, "pomeranians" -> 3,
      "akitas" -> 0, "vizslas" -> 0, "goldfish" -> 5, "trees" -> 3, "cars" -> 2, "perfumes" -> 1))
                                                  //> theRealSue  : day16.day16.Aunt = Aunt(theRealSue,Map(children -> 3, vizslas 
                                                  //| -> 0, trees -> 3, cars -> 2, akitas -> 0, goldfish -> 5, perfumes -> 1, pome
                                                  //| ranians -> 3, cats -> 7, samoyeds -> 2))

  aunts filter { _.props.forall { case (prop, value) => theRealSue.props(prop) == value } }
                                                  //> res0: List[day16.day16.Aunt] = List(Aunt(103,Map(cars -> 2, perfumes -> 1, 
                                                  //| goldfish -> 5)))

	def eq(x:Int)(y:Int):Boolean = y == x     //> eq: (x: Int)(y: Int)Boolean
	def gt(x:Int)(y:Int):Boolean = y > x      //> gt: (x: Int)(y: Int)Boolean
	def lt(x:Int)(y:Int):Boolean = y < x      //> lt: (x: Int)(y: Int)Boolean

  val matches2: Map[String, (Int => Boolean)] =
    Map("children" -> eq(3)_,
      "cats" -> gt(7)_,
      "samoyeds" -> eq(2)_,
      "pomeranians" -> lt(2)_,
      "akitas" -> eq(0)_,
      "vizslas" -> eq(0)_,
      "goldfish" -> lt(5)_,
      "trees" -> gt(3)_,
      "cars" -> eq(2)_,
      "perfumes" -> eq(1)_).withDefaultValue({_ => true})
                                                  //> matches2  : Map[String,Int => Boolean] = Map(children -> <function1>, vizsl
                                                  //| as -> <function1>, trees -> <function1>, cars -> <function1>, akitas -> <fu
                                                  //| nction1>, goldfish -> <function1>, perfumes -> <function1>, pomeranians -> 
                                                  //| <function1>, cats -> <function1>, samoyeds -> <function1>)

  aunts filter { _.props.forall { case (prop, value) => matches2(prop)(value) } }
                                                  //> res1: List[day16.day16.Aunt] = List(Aunt(405,Map(trees -> 8, perfumes -> 1,
                                                  //|  cars -> 2)))
}