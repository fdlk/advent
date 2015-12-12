package day12

import scala.util.parsing.json.JSON

object day12 {
  val dictionaryPath = List("day12", "day12.txt") //> dictionaryPath  : List[String] = List(day12, day12.txt)

  def lines = {
    val wordstream = Option {
      getClass.getClassLoader.getResourceAsStream(dictionaryPath.mkString("/"))
    } orElse {
      common.resourceAsStreamFromSrc(dictionaryPath)
    } getOrElse {
      sys.error("Could not load word list, dictionary file not found")
    }
    try {
      val s = io.Source.fromInputStream(wordstream)
      s.getLines.toList
    } catch {
      case e: Exception =>
        println("Could not load word list: " + e)
        throw e
    } finally {
      wordstream.close()
    }
  }                                               //> lines: => List[String]

  val input = lines(0)                            //> input  : String = [{"a":{"e":{"e":161,"a":"blue","d":{"e":-14,"a":"red","d":
                                                  //| {"c":"yellow","a":[-35,0],"b":"orange","d":{"e":70,"a":"green","d":"blue","j
                                                  //| ":12,"c":69,"h":"orange","b":92,"g":"yellow","f":"green","i":121}},"c":"blue
                                                  //| ","h":14,"b":46,"g":62,"f":[179]},"j":{"e":133,"c":"violet","a":"orange","b"
                                                  //| :"blue","d":"violet"},"c":{"e":151,"a":"violet","d":{"e":"red","c":93,"a":13
                                                  //| 5,"g":{"e":43,"c":"green","a":"orange","b":"green","d":54},"b":69,"d":159,"f
                                                  //| ":2},"c":"green","h":65,"b":{"c":126,"a":106,"b":190,"d":-40},"g":134,"f":"r
                                                  //| ed"},"h":87,"b":[-3,"yellow",50,120],"g":{"e":[138,83,"red"],"c":["yellow",[
                                                  //| "red",177,98,"blue",179,"blue","violet",23],108,["green",17,-46,3,99],-43,46
                                                  //| ,"orange","yellow",{"a":192,"b":39},57],"a":"red","b":195,"d":172},"f":97,"i
                                                  //| ":160},"a":"orange","d":120,"c":61,"h":"red","b":186,"g":{"e":"orange","a":8
                                                  //| 2,"d":{"a":{"e":"green","c":-5,"a":-13,"b":12,"d":"blue","f":-19}},"c":"blue
                                                  //| ","h":["violet","violet"
                                                  //| Output exceeds cutoff limit.

  val numbers = """-?[0-9]+""".r                  //> numbers  : scala.util.matching.Regex = -?[0-9]+

  numbers.findAllMatchIn(input).map { _.toString.toInt }.sum
                                                  //> res0: Int = 156366

  def valueOf(s: String): Int = s match {
    case numbers() => s.toInt
    case _         => 0
  }                                               //> valueOf: (s: String)Int

  def sum(tree: Any): Int = tree match {
    case map: Map[String, Any] if map.values.exists { _ == "red" } => 0
    case map: Map[String, Any] => {
      map.map({ case (k: String, v: Any) => valueOf(k) + sum(v) }).reduceLeft(_ + _)
    }
    case list: List[Any] => {
      list.map({ sum(_) }).reduceLeft(_ + _)
    }
    case s: String => valueOf(s)
    case d: Double => d.toInt
  }                                               //> sum: (tree: Any)Int

  sum(JSON.parseFull(input).get)                  //> res1: Int = 96852

}