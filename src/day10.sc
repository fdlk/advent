object day10 {
  def elvesSayInternal(s: List[Char], c: Char, i: Int, prefix: StringBuilder): String = s match {
    case Nil                 => prefix.append(i).append(c).toString
    case c2 :: s1 if c == c2 => elvesSayInternal(s1, c, i + 1, prefix)
    case c2 :: s1            => elvesSayInternal(s1, c2, 1, prefix.append(i).append(c))
  }

  def elvesSay(s: String, i:Int): String = {
    elvesSayInternal(s.substring(1).toList, s.charAt(0), 1, new StringBuilder())
  }

  (1 to 40).foldLeft("1321131112")(elvesSay).length
  (1 to 50).foldLeft("1321131112")(elvesSay).length
}