package day7

object day7 {
  val dictionaryPath = List("day7", "day7.txt")   //> dictionaryPath  : List[String] = List(day7, day7.txt)

  def loadPackets = {
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
  }                                               //> loadPackets: => List[String]

  def asUnsigned(unsignedLong: Int): Int = unsignedLong.toChar.toInt
                                                  //> asUnsigned: (unsignedLong: Int)Int

  type Signal = Int
  type State = Map[String, Signal]

  trait Eval {
    def eval(state: State): State
  }

  case class NumSignalGate(num: Int, name: String) extends Eval {
    override def eval(state: State): State = {
      state.updated(name, num)
    }
  }
  
  case class SignalGate(in: String, name: String) extends Eval {
    override def eval(state: State): State = {
      val newState: Option[State] = for {
        s <- state.get(in)
      } yield state.updated(name, s)
      newState.getOrElse(state)
    }
  }

  case class AndGate(in1: String, in2: String, out: String) extends Eval {
    override def eval(state: State): State = {
      val newState: Option[State] = for {
        s1 <- state.get(in1)
        s2 <- state.get(in2)
      } yield state.updated(out, asUnsigned(s1 & s2))
      newState.getOrElse(state)
    }
  }
  
  case class NumAndGate(s1: Int, in2: String, out: String) extends Eval {
    override def eval(state: State): State = {
      val newState: Option[State] = for {
        s2 <- state.get(in2)
      } yield state.updated(out, asUnsigned(s1 & s2))
      newState.getOrElse(state)
    }
  }

  case class OrGate(in1: String, in2: String, out: String) extends Eval {
    override def eval(state: State): State = {
      val newState: Option[State] = for {
        s1 <- state.get(in1)
        s2 <- state.get(in2)
      } yield state.updated(out, asUnsigned(s1 | s2))
      newState.getOrElse(state)
    }
  }

  case class RShiftGate(in: String, shift: Int, out: String) extends Eval {
    override def eval(state: State): State = {
      val newState: Option[State] = for {
        s1 <- state.get(in)
      } yield state.updated(out, asUnsigned(s1 >>> shift))
      newState.getOrElse(state)
    }
  }

  case class LShiftGate(in: String, shift: Int, out: String) extends Eval {
    override def eval(state: State): State = {
      val newState: Option[State] = for {
        s1 <- state.get(in)
      } yield state.updated(out, asUnsigned(s1 << shift))
      newState.getOrElse(state)
    }
  }

  case class NotGate(in: String, out: String) extends Eval {
    override def eval(state: State): State = {
      val newState: Option[State] = for {
        s1 <- state.get(in)
      } yield state.updated(out, asUnsigned(~s1))
      newState.getOrElse(state)
    }
  }

  val numSignal = """([0-9]+) -> ([a-z]+)""".r    //> numSignal  : scala.util.matching.Regex = ([0-9]+) -> ([a-z]+)
  val signal = """([a-z]+) -> ([a-z]+)""".r       //> signal  : scala.util.matching.Regex = ([a-z]+) -> ([a-z]+)
  val and = """([a-z]+) AND ([a-z]+) -> ([a-z]+)""".r
                                                  //> and  : scala.util.matching.Regex = ([a-z]+) AND ([a-z]+) -> ([a-z]+)
  
  val numAnd = """([0-9]+) AND ([a-z]+) -> ([a-z]+)""".r
                                                  //> numAnd  : scala.util.matching.Regex = ([0-9]+) AND ([a-z]+) -> ([a-z]+)
  val or = """([a-z]+) OR ([a-z]+) -> ([a-z]+)""".r
                                                  //> or  : scala.util.matching.Regex = ([a-z]+) OR ([a-z]+) -> ([a-z]+)
  val lshift = """([a-z]+) LSHIFT ([0-9]+) -> ([a-z]+)""".r
                                                  //> lshift  : scala.util.matching.Regex = ([a-z]+) LSHIFT ([0-9]+) -> ([a-z]+)
  val rshift = """([a-z]+) RSHIFT ([0-9]+) -> ([a-z]+)""".r
                                                  //> rshift  : scala.util.matching.Regex = ([a-z]+) RSHIFT ([0-9]+) -> ([a-z]+)
  val not = """NOT ([a-z]+) -> ([a-z]+)""".r      //> not  : scala.util.matching.Regex = NOT ([a-z]+) -> ([a-z]+)
  val gates = loadPackets map { x =>
    x match {
      case numSignal(num, name)    => NumSignalGate(num.toInt, name)
      case signal(in,out) => SignalGate(in, out)
      case and(in1, in2, out)   => AndGate(in1, in2, out)
      case numAnd(in1, in2, out)   => NumAndGate(in1.toInt, in2, out)
      case or(in1, in2, out)    => OrGate(in1, in2, out)
      case lshift(in, num, out) => LShiftGate(in, num.toInt, out)
      case rshift(in, num, out) => RShiftGate(in, num.toInt, out)
      case not(in, out)         => NotGate(in, out)
    }                                             //> gates  : List[Product with Serializable with day7.day7.Eval] = List(NotGate
                                                  //| (dq,dr), OrGate(kg,kf,kh), OrGate(ep,eo,eq), NumSignalGate(3176,b), NotGate
                                                  //| (gs,gt), OrGate(dd,do,dp), AndGate(eg,ei,ej), AndGate(y,ae,ag), AndGate(jx,
                                                  //| jz,ka), RShiftGate(lf,2,lg), AndGate(z,aa,ac), AndGate(dy,ej,el), OrGate(bj
                                                  //| ,bi,bk), RShiftGate(kk,3,km), NotGate(cn,co), AndGate(gn,gp,gq), AndGate(cq
                                                  //| ,cs,ct), LShiftGate(eo,15,es), OrGate(lg,lm,ln), OrGate(dy,ej,ek), NotGate(
                                                  //| di,dj), NumAndGate(1,fi,fj), LShiftGate(kf,15,kj), NotGate(jy,jz), NotGate(
                                                  //| ft,fu), AndGate(fs,fu,fv), NotGate(hr,hs), OrGate(ck,cl,cm), RShiftGate(jp,
                                                  //| 5,js), OrGate(iv,jb,jc), OrGate(is,it,iu), OrGate(ld,le,lf), NotGate(fc,fd)
                                                  //| , NotGate(dm,dn), OrGate(bn,by,bz), AndGate(aj,al,am), LShiftGate(cd,15,ch)
                                                  //| , AndGate(jp,ka,kc), OrGate(ci,ct,cu), AndGate(gv,gx,gy), AndGate(de,dk,dm)
                                                  //| , RShiftGate(x,5,aa), RShiftGate(et,2,eu), RShiftGate(x,1,aq), OrGate(ia,ig
                                                  //| ,ih), LShiftGate(bk,1,c
                                                  //| Output exceeds cutoff limit.
  }
  

	def reduce(s:State, e:Eval): State = {
		e.eval(s)
	}                                         //> reduce: (s: day7.day7.State, e: day7.day7.Eval)day7.day7.State
  def update(s: State): State = {
 	 	val updated = gates.foldLeft(s) (reduce)
 	 	if( s != updated ) update (updated) else updated
  }                                               //> update: (s: day7.day7.State)day7.day7.State

	update(Map()) ("a")                       //> res0: day7.day7.Signal = 14710
}