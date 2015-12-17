import scala.util.parsing.combinator.JavaTokenParsers

object day16 {

  case class Aunt(sue: String, props: Map[String, Int])

  class AuntParser extends JavaTokenParsers {
    def thing: Parser[String] = "children" | "cats" | "samoyeds" | "akitas" | "vizslas" | "goldfish" | "trees" | "cars" | "perfumes" | "pomeranians" ^^ { x => x }
    def aunt: Parser[Aunt] = "Sue " ~> (wholeNumber <~ ":") ~ repsep(property, ",") ^^ { case sue ~ props => Aunt(sue, props.toMap) }
    def property: Parser[(String, Int)] = (thing <~ ":") ~ wholeNumber ^^ { case name ~ value => (name, value.toInt) }
  }
  object AuntParser extends AuntParser

  val aunts = common.loadPackets(List("day16", "day16.txt")) map { AuntParser.parseAll(AuntParser.aunt, _).get }
  val theRealSue = Aunt("theRealSue",
    Map("children" -> 3, "cats" -> 7, "samoyeds" -> 2, "pomeranians" -> 3,
      "akitas" -> 0, "vizslas" -> 0, "goldfish" -> 5, "trees" -> 3, "cars" -> 2, "perfumes" -> 1))

  aunts filter { _.props.forall { case (prop, value) => theRealSue.props(prop) == value } }

  val matches2: Map[String, (Int => Boolean)] =
    Map("children" -> { 3 == _ },
      "cats" -> {7 < _},
      "samoyeds" -> { 2 == _ },
      "pomeranians" -> {2 > _},
      "akitas" -> {0 == _},
      "vizslas" -> {0 == _},
      "goldfish" -> {5 > _},
      "trees" -> {3 < _},
      "cars" -> {2 == _},
      "perfumes" -> {1 == _}).withDefaultValue({_ => true})

  aunts filter { _.props.forall { case (prop, value) => matches2(prop)(value) } }
}