import scala.util.parsing.combinator.JavaTokenParsers
object day13 {
  type Guest = String

  case class Relationship(guests: Set[Guest], happiness: Int)

  class RelationshipParser extends JavaTokenParsers {
    def guest: Parser[Guest] = """\w+""".r

    def rule: Parser[Relationship] =
      (guest <~ "would") ~ ("gain" | "lose") ~ wholeNumber ~
        ("happiness units by sitting next to" ~> guest <~ ".") ^^ { case guest1 ~ effect ~ happiness ~ guest2 =>
        effect match {
          case "gain" => Relationship(Set(guest1, guest2), happiness.toInt)
          case "lose" => Relationship(Set(guest1, guest2), -happiness.toInt)
        }
      }
  }

  object RelationshipParser extends RelationshipParser
  val relationships = common.loadPackets(List("day13", "day13.txt"))
    .map(RelationshipParser.parseAll(RelationshipParser.rule, _).get)
  val guests = relationships.flatMap(_.guests).toSet

  def happiness(combination: List[Guest]): Int = {
    val circle = combination ::: (combination take 1)
    (for {
      pair <- circle.sliding(2)
      relationship <- relationships.filter(_.guests == pair.toSet)
    } yield relationship.happiness).sum
  }

  val sol1 = guests.toList.permutations map happiness max
  val sol2 = (guests + "Fleur").toList.permutations map happiness max
}