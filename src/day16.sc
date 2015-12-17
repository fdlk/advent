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

  def matches2(prop: (String, Int)): Boolean = prop match {
    case ("children", x) => x == 3
    case ("cats", x) => x > 7
    case ("samoyeds", x) => x == 2
    case ("pomeranians", x) => x < 2
    case ("akitas", x) => x == 0
    case ("vizslas", x) => x == 0
    case ("goldfish", x) => x < 5
    case ("trees", x) => x > 3
    case ("cars", x) => x == 2
    case ("perfumes", x) => x == 1
    case _ => true
  }

  aunts filter { _.props forall  matches2 }
}