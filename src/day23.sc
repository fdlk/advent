object day23 {
  val input: List[String] = common.loadPackets(List("day23.txt"))
  case class Registers(a: Long, b: Long, ip: Int) {
    def hlf(ab: Char) = ab match {
      case 'a' => Registers(a / 2, b, ip + 1)
      case 'b' => Registers(a, b / 2, ip + 1)
    }
    def tpl(ab: Char) = ab match {
        case 'a' => Registers(a * 3, b, ip + 1)
        case 'b' => Registers(a, b * 3, ip + 1)
    }
    def inc(ab: Char) = ab match {
      case 'a' => Registers(a + 1, b, ip + 1)
      case 'b' => Registers(a, b + 1, ip + 1)
    }
    def jmp(offset: Int) = Registers(a, b, ip + offset)
    def jie(ab: Char, offset: Int) = ab match {
      case 'a' => if (a % 2 == 0) jmp(offset) else jmp(1)
      case 'b' => if (b % 2 == 0) jmp(offset) else jmp(1)
    }
    def jio(ab: Char, offset: Int) = ab match {
      case 'a' => if (a == 1) jmp(offset) else jmp(1)
      case 'b' => if (b == 1) jmp(offset) else jmp(1)
    }
  }
  val hlfR = """hlf ([ab])""".r
  val tplR = """tpl ([ab])""".r
  val incR = """inc ([ab])""".r
  val jmpR = """jmp ([+-][0-9]+)""".r
  val jieR = """jie ([ab]), ([+-][0-9]+)""".r
  val jioR = """jio ([ab]), ([+-][0-9]+)""".r

  def interpret(r: Registers): Option[Registers] = {
    try {
      val instruction = input(r.ip)
      instruction match {
        case hlfR(ab) => Some(r.hlf(ab.charAt(0)))
        case tplR(ab) => Some(r.tpl(ab.charAt(0)))
        case incR(ab) => Some(r.inc(ab.charAt(0)))
        case jmpR(offset) => Some(r.jmp(offset.toInt))
        case jieR(ab, offset) => Some(r.jie(ab.charAt(0), offset.toInt))
        case jioR(ab, offset) => Some(r.jio(ab.charAt(0), offset.toInt))
        case _ => None
      }
    }
    catch {
      case e: IndexOutOfBoundsException => None
    }
  }

  def result(r: Registers): Registers = {
    interpret(r) match {
      case Some(upd) => result(upd)
      case None => r
    }
  }

  result(Registers(0,0,0))
  result(Registers(1,0,0))
}