import scala.io.Source
import scala.util.parsing.combinator.JavaTokenParsers

val input = Source.fromInputStream(getClass.getResourceAsStream("day12/day12.txt")).mkString

object JsonParser extends JavaTokenParsers {
  def obj = "{" ~> repsep(member, ",") <~ "}" ^^ { l => if (l contains None) 0 else l.flatten.sum }
  def arr = "[" ~> repsep(value, ",") <~ "]" ^^ {_.flatten.sum}
  def member = (stringLiteral <~ ":") ~> value
  def value: Parser[Option[Int]] = ((obj | arr) ^^ {Some(_)}
    | stringLiteral ^^ { l => if (l == "\"red\"") None else Some(0) }
    | floatingPointNumber ^^ { x => Some(x.toInt) })
}
JsonParser.parseAll(JsonParser.value, input)