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
  }                                               //> reduce: (l: day6.day6.Lighting, s: String)day6.day6.Lighting

  val start: Lighting = Dark();                   //> start  : day6.day6.Lighting = Dark()
  val inputs = common.loadPackets(List("day6", "day6.txt"));
                                                  //> inputs  : List[String] = List(turn off 660,55 through 986,197, turn off 341
                                                  //| ,304 through 638,850, turn off 199,133 through 461,193, toggle 322,558 thro
                                                  //| ugh 977,958, toggle 537,781 through 687,941, turn on 226,196 through 599,39
                                                  //| 0, turn on 240,129 through 703,297, turn on 317,329 through 451,798, turn o
                                                  //| n 957,736 through 977,890, turn on 263,530 through 559,664, turn on 158,270
                                                  //|  through 243,802, toggle 223,39 through 454,511, toggle 544,218 through 979
                                                  //| ,872, turn on 313,306 through 363,621, toggle 173,401 through 496,407, togg
                                                  //| le 333,60 through 748,159, turn off 87,577 through 484,608, turn on 809,648
                                                  //|  through 826,999, toggle 352,432 through 628,550, turn off 197,408 through 
                                                  //| 579,569, turn off 1,629 through 802,633, turn off 61,44 through 567,111, to
                                                  //| ggle 880,25 through 903,973, turn on 347,123 through 864,746, toggle 728,87
                                                  //| 7 through 996,975, turn on 121,895 through 349,906, turn on 888,547 through
                                                  //|  931,628, toggle 398,78
                                                  //| Output exceeds cutoff limit.
  val finalState =  inputs.foldLeft(start)(reduce)//> finalState  : day6.day6.Lighting = Toggle(296,687,906,775,Toggle(580,592,67
                                                  //| 1,900,Toggle(424,675,740,862,TurnOn(715,871,722,890,TurnOff(446,432,458,648
                                                  //| ,TurnOn(20,984,571,994,TurnOn(777,812,837,912,Toggle(83,575,915,728,TurnOff
                                                  //| (50,197,733,656,Toggle(66,191,757,481,Toggle(475,711,921,882,TurnOn(717,272
                                                  //| ,850,817,Toggle(871,952,989,998,TurnOff(441,375,974,545,Toggle(602,45,763,1
                                                  //| 51,TurnOn(627,796,973,940,Toggle(319,184,382,203,Toggle(112,414,387,421,Tog
                                                  //| gle(629,387,814,577,TurnOn(445,611,532,705,TurnOff(235,899,818,932,Toggle(1
                                                  //| 07,322,378,688,Toggle(767,98,854,853,TurnOff(376,528,779,640,TurnOn(271,845
                                                  //| ,454,882,TurnOff(363,899,948,935,TurnOn(519,600,750,615,TurnOn(969,994,983,
                                                  //| 997,Toggle(384,994,663,999,TurnOn(113,340,472,972,Toggle(161,105,657,395,To
                                                  //| ggle(501,412,998,516,TurnOn(165,753,202,780,TurnOff(701,636,928,877,Toggle(
                                                  //| 441,215,528,680,TurnOff(760,75,800,275,TurnOff(974,230,995,641,TurnOff(598,
                                                  //| 681,978,921,TurnOn(445,
                                                  //| Output exceeds cutoff limit.

  val lights = for {
    x <- 0 until 1000
    y <- 0 until 1000
  } yield finalState.brightness(x, y)             //> lights  : scala.collection.immutable.IndexedSeq[Int] = Vector(0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 
                                                  //| Output exceeds cutoff limit.

  lights.sum                                      //> res0: Int = 15343601
}