object day6 {

  trait Lighting {
    def isLit(x: Int, y: Int): Boolean
    def brightness(x: Int, y: Int): Int
  }

  case class Dark() extends Lighting {
    override def isLit(x: Int, y: Int) = false
    override def brightness(x: Int, y: Int) = 0
  }

  trait Square {
    val x0: Int
    val y0: Int
    val x1: Int
    val y1: Int
    def contains(x: Int, y: Int): Boolean = x0 <= x && x <= x1 && y0 <= y && y <= y1
  }

  case class TurnOff(x0: Int, y0: Int, x1: Int, y1: Int, s: Lighting) extends Square with Lighting {
    override def isLit(x: Int, y: Int) = if (contains(x, y)) false else s.isLit(x, y)
    override def brightness(x: Int, y: Int) = if (contains(x, y)) Math.max(s.brightness(x, y) - 1, 0) else s.brightness(x, y)
  }

  case class TurnOn(x0: Int, y0: Int, x1: Int, y1: Int, s: Lighting) extends Square with Lighting {
    override def isLit(x: Int, y: Int) = if (contains(x, y)) true else s.isLit(x, y)
    override def brightness(x: Int, y: Int) = if (contains(x, y)) s.brightness(x, y) + 1 else s.brightness(x, y)
  }

  case class Toggle(x0: Int, y0: Int, x1: Int, y1: Int, s: Lighting) extends Square with Lighting {
    override def isLit(x: Int, y: Int) = if (contains(x, y)) !s.isLit(x, y) else s.isLit(x, y)
    override def brightness(x: Int, y: Int) = if (contains(x, y)) s.brightness(x, y) + 2 else s.brightness(x, y)
  }

  def reduce(l: Lighting, s: String): Lighting = {
    val turnOff = """turn off (\d{0,3}),(\d{0,3}) through (\d{0,3}),(\d{0,3})""".r
    val turnOn = """turn on (\d{0,3}),(\d{0,3}) through (\d{0,3}),(\d{0,3})""".r
    val toggle = """toggle (\d{0,3}),(\d{0,3}) through (\d{0,3}),(\d{0,3})""".r
    s match {
      case turnOff(x0, y0, x1, y1) => TurnOff(x0.toInt, y0.toInt, x1.toInt, y1.toInt, l)
      case turnOn(x0, y0, x1, y1)  => TurnOn(x0.toInt, y0.toInt, x1.toInt, y1.toInt, l)
      case toggle(x0, y0, x1, y1)  => Toggle(x0.toInt, y0.toInt, x1.toInt, y1.toInt, l)
    }
  }
  val start: Lighting = Dark()
  val inputs = common.loadPackets(List("day6", "day6.txt"))
  val finalState =  inputs.foldLeft(start)(reduce)
  val lights1 = for {
    x <- 0 until 1000
    y <- 0 until 1000
    if finalState.isLit(x, y)
  } yield 1
  lights1.length

  val lights2 = for {
    x <- 0 until 1000
    y <- 0 until 1000
  } yield finalState.brightness(x, y)
  lights2.sum
}