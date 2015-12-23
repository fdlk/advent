import scala.util.parsing.combinator.JavaTokenParsers
object day23 {
  case class Regs(a: Long, b: Long, ip: Int) {
    def update(ab: Char, f: Long => Long): Regs = ab match {
      case 'a' => copy(a=f(a)).jmp(1)
      case 'b' => copy(b=f(b)).jmp(1)
    }
    def jmp(offset: Int) = copy(ip=ip+offset)
    def read(ab: Char): Long = if(ab == 'a') a else b
  }
  type Instruction = (Regs=>Regs)
  class InstructionParser extends JavaTokenParsers {
    def register: Parser[Char] = ("a"| "b")           ^^ {_.charAt(0)}
    def offset: Parser[Int] = opt("+") ~> wholeNumber ^^ {_.toInt}
    def hlf = "hlf" ~> register                       ^^ { ab             => r:Regs => r.update(ab, _/2) }
    def tpl = "tpl" ~> register                       ^^ { ab             => r:Regs => r.update(ab, _*3) }
    def inc = "inc" ~> register                       ^^ { ab             => r:Regs => r.update(ab, _+1) }
    def jmp = "jmp" ~> offset                         ^^ { offset         => r:Regs => r.jmp(offset) }
    def jie = "jie" ~> register ~ ("," ~> offset)     ^^ { case ab~offset => r:Regs => r.jmp(if(r.read(ab) % 2 == 0) offset else 1)}
    def jio = "jio" ~> register ~ ("," ~> offset)     ^^ { case ab~offset => r:Regs => r.jmp(if(r.read(ab) == 1)     offset else 1)}
    def instruction: Parser[Instruction] = hlf | tpl | inc | jmp | jio | jie
  }
  object InstructionParser extends InstructionParser
  def execute(program: List[Instruction], initialState: Regs): Regs = {
    Stream.iterate(initialState)(r => program(r.ip)(r))
      .find(r => !program.indices.contains(r.ip)).get
  }
  val program:List[Instruction] = common.loadPackets(List("day23.txt"))
    .map(line => InstructionParser.parseAll(InstructionParser.instruction, line).get)
  execute(program, Regs(0,0,0))
  execute(program, Regs(1,0,0))
}