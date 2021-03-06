object day11 {
  def next(c: Char) = (c + 1).toChar

  def hasThreeInARow(cs: List[Char]): Boolean = cs match {
    case Nil => false
    case c1 :: c2 :: c3 :: rest if next(c1) == c2 && next(c2) == c3 => true
    case c1 :: rest => hasThreeInARow(rest)
  }

  def nextReversed(cs: List[Char]): List[Char] = cs match {
    case c1 :: rest if c1 == 'z' => 'a' :: nextReversed(rest)
    case c1 :: rest => next(c1) :: rest
  }

  def hasNoInvalidLetter(s: List[Char]): Boolean = s.forall {
    !"iol".contains(_)
  }

  def hasPair(cs: List[Char]): Boolean = {
    val x = cs.zipWithIndex.sliding(2).filter { x => x.length > 1 && x(0)._1 == x(1)._1 }.map {
      _ (0)._2
    }.toList
    x.nonEmpty && x.max - x.min > 1
  }

  def nextString(cs: List[Char]): List[Char] = nextReversed(cs.reverse).reverse

  def find(s: List[Char]): List[Char] = {
    if (hasThreeInARow(s) && hasNoInvalidLetter(s) && hasPair(s)) s
    else find(nextString(s))
  }

  "ghjaabcc".toList.grouped(2).filter { x => x.length > 1 && x(0) == x(1) }.toList
  find(nextString("cqjxxyzz".toList)).mkString
}