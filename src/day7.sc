import scala.util.parsing.combinator.JavaTokenParsers

object day17 {
  type Signal = Int
  type State = Map[String, Signal]

  trait Rule {
    def evaluate(s: State): Option[Signal]
  }

  case class Constant(value: Signal) extends Rule {
    override def evaluate(state: State) = Some(value)
  }

  case class Connect(in: String) extends Rule {
    override def evaluate(state: State) =
      for {s <- state.get(in)} yield s
  }

  case class And(in1: String, in2: String) extends Rule {
    override def evaluate(state: State) =
      for {s1 <- state.get(in1)
           s2 <- state.get(in2)}
        yield s1 & s2
  }

  case class AndConstant(s1: Signal, in: String) extends Rule {
    override def evaluate(state: State) =
      for {s2 <- state.get(in)}
        yield s1 & s2
  }

  case class Or(in1: String, in2: String) extends Rule {
    override def evaluate(state: State) =
      for {s1 <- state.get(in1)
           s2 <- state.get(in2)}
        yield s1 | s2
  }

  case class Not(in: String) extends Rule {
    override def evaluate(state: State) =
      for {s <- state.get(in)}
        yield ~s
  }

  case class RShift(in: String, shift: Int) extends Rule {
    override def evaluate(state: State) =
      for {signal <- state.get(in)}
        yield signal >>> shift
  }

  case class LShift(in: String, shift: Int) extends Rule {
    override def evaluate(state: State) =
      for {signal <- state.get(in)}
        yield signal << shift
  }

  class WireParser extends JavaTokenParsers {
    def signal: Parser[Int] = wholeNumber ^^ {
      _.toInt
    }

    def connector: Parser[String] =
      """[a-z]{1,2}""".r ^^ { x => {
        println(x);
        x
      }
      }

    def connection: Parser[(String, Rule)] = (rule <~ "->") ~ connector ^^ { case rule ~ connector => (connector, rule) }

    def rule: Parser[Rule] = and | andConstant | or | lshift | rshift | not | constant | connect

    def constant: Parser[Constant] = wholeNumber ^^ { x => Constant(x.toInt) }

    def connect: Parser[Connect] = connector ^^ { x => Connect(x) }

    def and: Parser[And] = (connector <~ "AND") ~ connector ^^ { case in1 ~ in2 => And(in1, in2) }

    def andConstant: Parser[AndConstant] = (signal <~ "AND") ~ connector ^^ { case s ~ in => AndConstant(s, in) }

    def or: Parser[Or] = (connector <~ "OR") ~ connector ^^ { case in1 ~ in2 => Or(in1, in2) }

    def not: Parser[Not] = "NOT" ~> connector ^^ { case in => Not(in) }

    def lshift: Parser[LShift] = (connector <~ "LSHIFT") ~ signal ^^ { case in ~ shift => LShift(in, shift) }

    def rshift: Parser[RShift] = (connector <~ "RSHIFT") ~ signal ^^ { case in ~ shift => RShift(in, shift) }
  }

  object WireParser extends WireParser

  val input = common.loadPackets(List("day7", "day7.txt"))
  val rules = input.map { line => {
    WireParser.parse(WireParser.connection, line).get
  }
  }

  def reduce(state: State, e: (String, Rule)): State = {
    (e match {
      case (name, r: Rule) =>
        for {value <- r.evaluate(state)
             unsignedValue = value.toChar.toInt}
          yield state.updated(name, unsignedValue)
    }).getOrElse(state)
  }

  def update(state: State): State = {
    val updated = rules.foldLeft(state)(reduce)
    if (state != updated) update(updated) else updated
  }

  update(Map())("a")
}