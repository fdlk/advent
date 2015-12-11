package day11

object day11 {
  def next(c: Char) = (c+ 1).toChar               //> next: (c: Char)Char

  def hasThreeInARowChars(cs: List[Char]): Boolean = cs match {
 		case Nil => false
    case c1 :: c2 :: c3 :: rest if next(c1) == c2 && next(c2) == c3 => true
    case c1 :: rest => hasThreeInARowChars(rest)
  }                                               //> hasThreeInARowChars: (cs: List[Char])Boolean

  def hasThreeInARow(cs: String): Boolean = hasThreeInARowChars(cs.toList)
                                                  //> hasThreeInARow: (cs: String)Boolean

  hasThreeInARow("zabz")                          //> res0: Boolean = false

  def nextReversed(cs: List[Char]): List[Char] = cs match {
    case c1 :: rest if c1 == 'z' => 'a' :: nextReversed(rest)
    case c1 :: rest              => next(c1) :: rest
  }                                               //> nextReversed: (cs: List[Char])List[Char]

	def hasNoInvalidLetter(s: String): Boolean = s.toList.forall { ! "iol".contains(_) }
                                                  //> hasNoInvalidLetter: (s: String)Boolean

	def hasPair(cs: List[Char]): Boolean = {
		val x = cs.zipWithIndex.sliding(2).filter { x => x.length > 1 && x(0)._1 == x(1)._1 }.map {_(0)._2}.toList
		x.nonEmpty && x.max - x.min > 1
	}                                         //> hasPair: (cs: List[Char])Boolean
	
	def hasPairString(cs: String): Boolean = hasPair(cs.toList)
                                                  //> hasPairString: (cs: String)Boolean

  def nextString(cs: String): String = nextReversed(cs.toList.reverse).reverse.mkString
                                                  //> nextString: (cs: String)String

	hasPairString("abccde")                   //> res1: Boolean = false
 
	def find(s: String) : String = {
		if(hasThreeInARow(s)
		&& hasNoInvalidLetter(s)
		&& hasPairString(s)) s else find(nextString(s))
	}                                         //> find: (s: String)String
	
	"ghjaabcc".toList.grouped(2).filter { x => x.length > 1 && x(0) == x(1) }.toList
                                                  //> res2: List[List[Char]] = List(List(c, c))
	
	hasThreeInARow("ghjaabcc")                //> res3: Boolean = true
	hasNoInvalidLetter("ghjaabcc")            //> res4: Boolean = true
	hasPairString("ghjaabcc")                 //> res5: Boolean = true
	find(nextString("cqjxxyzz"))              //> res6: String = cqkaabcc
}