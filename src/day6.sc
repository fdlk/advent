import scala.util.parsing.combinator.JavaTokenParsers

object day6 {

  trait Lighting {
    def isLit(previous: Boolean): Boolean
    def brightness(previous: Int): Int
  }

  case class Square(xRange: Range, yRange: Range) {
    def contains(x: Int, y: Int): Boolean =
      xRange.contains(x) && yRange.contains(y)
  }

  object Dark extends Lighting {
    override def isLit(previous: Boolean) = false
    override def brightness(previous: Int) = 0
  }

  object TurnOff extends Lighting {
    override def isLit(previous: Boolean) = false
    override def brightness(previous: Int) = Math.max(previous - 1, 0)
  }

  object TurnOn extends Lighting {
    override def isLit(previous: Boolean) = true
    override def brightness(previous: Int) = previous + 1
  }

  object Toggle extends Lighting {
    override def isLit(previous: Boolean) = !previous
    override def brightness(previous: Int) = previous + 2
  }

  case class Instruction(lighting: Lighting, square: Square)

  class InstructionParser extends JavaTokenParsers {
    def point: Parser[(Int, Int)] = {wholeNumber ~ "," ~ wholeNumber ^^ { case x ~ "," ~ y => (x.toInt, y.toInt) }}
    def square: Parser[Square] = point ~ "through" ~ point ^^ { case p1 ~ "through" ~ p2 => Square(p1._1 to p2._1, p1._2 to p2._2) }
    def lighting: Parser[Lighting] = "turn off" ^^ {case _ => TurnOff} | "turn on" ^^ {case _ => TurnOn} | "toggle" ^^ {case _ => Toggle}
    def instruction: Parser[Instruction] = lighting~square ^^ {case lighting~square => Instruction(lighting, square)}
  }
  object InstructionParser extends InstructionParser
  val start: Lighting = Dark
  val instructions = common.loadPackets(List("day6", "day6.txt"))
    .map(InstructionParser.parseAll(InstructionParser.instruction, _).get)

  def relevantInstructions(x: Int, y: Int) = for {
    instruction <- instructions
    if instruction.square.contains(x, y)
  } yield instruction

  lazy val relevantInstructionsPerSquare = for {
    x <- 0 until 1000
    y <- 0 until 1000
  } yield relevantInstructions(x, y)

  def isLit(lit: Boolean, instruction: Instruction) = instruction.lighting.isLit(lit)
  def howBright(brightness: Int, instruction: Instruction) = instruction.lighting.brightness(brightness)

  relevantInstructionsPerSquare.map {
    _.foldLeft(false)(isLit)
  }.count(identity)

  relevantInstructionsPerSquare.map {
    _.foldLeft(0)(howBright)
  }.sum
}