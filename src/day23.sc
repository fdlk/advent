import scala.util.parsing.combinator.JavaTokenParsers
object day23 {
  case class Registers(a: Long, b: Long, ip: Int) {
    def update(ab: Char, f: Long => Long): Registers = ab match {
      case 'a' => Registers(f(a), b, ip+1)
      case 'b' => Registers(a, f(b), ip+1)
    }
    def jmp(offset: Int) = Registers(a, b, ip + offset)
    def read(ab: Char): Long = ab match {case 'a' => a; case 'b' => b}
  }
  type Instruction = (Registers=>Registers)
  class InstructionParser extends JavaTokenParsers {
    def register: Parser[Char] = ("a"| "b") ^^ {_.charAt(0)}
    def offset: Parser[Int] = opt("+")~>wholeNumber ^^ {_.toInt}
    def instruction: Parser[Instruction] = hlf | tpl | inc | jmp | jio | jie
    def hlf = "hlf"~>register ^^ { ab => r:Registers => r.update(ab, _/2) }
    def tpl = "tpl"~>register ^^ { ab => r:Registers => r.update(ab, _*3) }
    def inc = "inc"~>register ^^ { ab => r:Registers => r.update(ab, _+1) }
    def jmp = "jmp"~>offset ^^ { offset => r:Registers => r.jmp(offset) }
    def jie = "jie"~>register~(","~>offset) ^^ {
      case ab~offset => r:Registers =>
        if(r.read(ab) % 2 == 0)
          r.jmp(offset.toInt)
        else r.jmp(1)
    }
    def jio = "jio"~>register~(","~>offset) ^^ {
      case ab~offset => r:Registers =>
        if(r.read(ab) == 1)
          r.jmp(offset.toInt)
        else r.jmp(1)
    }
  }
  object InstructionParser extends InstructionParser
  def execute(program: List[Instruction], initialState: Registers): Registers = {
    Stream.iterate(initialState)(r => program(r.ip)(r)).find(r => !program.indices.contains(r.ip)).get
  }
  val program:List[Instruction] = common.loadPackets(List("day23.txt"))
    .map(line => InstructionParser.parseAll(InstructionParser.instruction, line).get)
  execute(program, Registers(0,0,0))
  execute(program, Registers(1,0,0))
}