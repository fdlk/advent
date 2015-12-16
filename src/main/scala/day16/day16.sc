package day16

object day16 {
  case class Aunt(name: String, props: Map[String, Int])

  def matches1(aunt: Aunt): Boolean = {
    (aunt.props.getOrElse("children", 3) == 3
      && aunt.props.getOrElse("cats", 7) == 7
      && aunt.props.getOrElse("samoyeds", 2) == 2
      && aunt.props.getOrElse("pomeranians", 3) == 3
      && aunt.props.getOrElse("akitas", 0) == 0
      && aunt.props.getOrElse("vizslas", 0) == 0
      && aunt.props.getOrElse("goldfish", 5) == 5
      && aunt.props.getOrElse("trees", 3) == 3
      && aunt.props.getOrElse("cars", 2) == 2
      && aunt.props.getOrElse("perfumes", 1) == 1)
  }                                               //> matches1: (aunt: day16.day16.Aunt)Boolean

  def matches2(aunt: Aunt): Boolean = {
    (aunt.props.getOrElse("children", 3) == 3
      && aunt.props.getOrElse("cats", 8) > 7
      && aunt.props.getOrElse("samoyeds", 2) == 2
      && aunt.props.getOrElse("pomeranians", 2) < 3
      && aunt.props.getOrElse("akitas", 0) == 0
      && aunt.props.getOrElse("vizslas", 0) == 0
      && aunt.props.getOrElse("goldfish", 4) < 5
      && aunt.props.getOrElse("trees", 4) > 3
      && aunt.props.getOrElse("cars", 2) == 2
      && aunt.props.getOrElse("perfumes", 1) == 1)
  }                                               //> matches2: (aunt: day16.day16.Aunt)Boolean

  val aunts = common.loadPackets(List("day16", "day16.txt")).map {
    _.split(":", 2) match {
      case Array(sue, props) =>
        Aunt(sue, props.split(",").map(_.split(":") match {
          case Array(k, v) => (k.trim, v.trim.toInt)
        }).toMap)
    }
  }                                               //> aunts  : List[day16.day16.Aunt] = List(Aunt(Sue 1,Map(goldfish -> 6, trees 
                                                  //| -> 9, akitas -> 0)), Aunt(Sue 2,Map(goldfish -> 7, trees -> 1, akitas -> 0)
                                                  //| ), Aunt(Sue 3,Map(cars -> 10, akitas -> 6, perfumes -> 7)), Aunt(Sue 4,Map(
                                                  //| perfumes -> 2, vizslas -> 0, cars -> 6)), Aunt(Sue 5,Map(goldfish -> 1, tre
                                                  //| es -> 3, perfumes -> 10)), Aunt(Sue 6,Map(children -> 9, vizslas -> 7, cars
                                                  //|  -> 9)), Aunt(Sue 7,Map(cars -> 6, vizslas -> 5, cats -> 3)), Aunt(Sue 8,Ma
                                                  //| p(akitas -> 10, vizslas -> 9, children -> 3)), Aunt(Sue 9,Map(vizslas -> 8,
                                                  //|  cats -> 2, trees -> 1)), Aunt(Sue 10,Map(perfumes -> 10, trees -> 6, cars 
                                                  //| -> 4)), Aunt(Sue 11,Map(cars -> 9, children -> 1, cats -> 1)), Aunt(Sue 12,
                                                  //| Map(pomeranians -> 4, akitas -> 6, goldfish -> 8)), Aunt(Sue 13,Map(cats ->
                                                  //|  10, children -> 5, trees -> 9)), Aunt(Sue 14,Map(perfumes -> 8, vizslas ->
                                                  //|  3, samoyeds -> 1)), Aunt(Sue 15,Map(vizslas -> 2, perfumes -> 8, trees -> 
                                                  //| 3)), Aunt(Sue 16,Map(po
                                                  //| Output exceeds cutoff limit.
  aunts filter matches1                           //> res0: List[day16.day16.Aunt] = List(Aunt(Sue 103,Map(cars -> 2, perfumes ->
                                                  //|  1, goldfish -> 5)))
  aunts filter matches2                           //> res1: List[day16.day16.Aunt] = List(Aunt(Sue 405,Map(trees -> 8, perfumes -
                                                  //| > 1, cars -> 2)))
}