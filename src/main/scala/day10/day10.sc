package day10

object day10 {
  def elvesSayInternal(s: List[Char], c: Char, i: Int, prefix: String): String = s match {
    case Nil                 => prefix + "" + i + c
    case c2 :: s1 if c == c2 => elvesSayInternal(s1, c, i + 1, prefix)
    case c2 :: s1            => elvesSayInternal(s1, c2, 1, prefix + "" + i + c)
  }                                               //> elvesSayInternal: (s: List[Char], c: Char, i: Int, prefix: String)String

  def elvesSay(s: String): String = {
    s.toList match {
      case c :: s1 => elvesSayInternal(s1, c, 1, "")
    }
  }                                               //> elvesSay: (s: String)String
  
  elvesSay("111221")                              //> res0: String = 312211
  
  val start: String = "1321131112"                //> start  : String = 1321131112

	def reduce ( s: String, i:Int) = {
		println(i)
		elvesSay(s)
	}                                         //> reduce: (s: String, i: Int)String

  ((1 to 50).foldLeft (start)(reduce)).size       //> 1
                                                  //| 2
                                                  //| 3
                                                  //| 4
                                                  //| 5
                                                  //| 6
                                                  //| 7
                                                  //| 8
                                                  //| 9
                                                  //| 10
                                                  //| 11
                                                  //| 12
                                                  //| 13
                                                  //| 14
                                                  //| 15
                                                  //| 16
                                                  //| 17
                                                  //| 18
                                                  //| 19
                                                  //| 20
                                                  //| 21
                                                  //| 22
                                                  //| 23
                                                  //| 24
                                                  //| 25
                                                  //| 26
                                                  //| 27
                                                  //| 28
                                                  //| 29
                                                  //| 30
                                                  //| 31
                                                  //| 32
                                                  //| 33
                                                  //| 34
                                                  //| 35
                                                  //| 36
                                                  //| 37
                                                  //| 38
                                                  //| 39
                                                  //| 40
                                                  //| res1: Int = 492982
}