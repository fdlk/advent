package day7

import common._

object day7 {
  val wires = common.loadPackets(List("day7", "day7.txt") )
                                                  //> wires  : List[String] = List(NOT dq -> dr, kg OR kf -> kh, ep OR eo -> eq, 3
                                                  //| 176 -> b, NOT gs -> gt, dd OR do -> dp, eg AND ei -> ej, y AND ae -> ag, jx 
                                                  //| AND jz -> ka, lf RSHIFT 2 -> lg, z AND aa -> ac, dy AND ej -> el, bj OR bi -
                                                  //| > bk, kk RSHIFT 3 -> km, NOT cn -> co, gn AND gp -> gq, cq AND cs -> ct, eo 
                                                  //| LSHIFT 15 -> es, lg OR lm -> ln, dy OR ej -> ek, NOT di -> dj, 1 AND fi -> f
                                                  //| j, kf LSHIFT 15 -> kj, NOT jy -> jz, NOT ft -> fu, fs AND fu -> fv, NOT hr -
                                                  //| > hs, ck OR cl -> cm, jp RSHIFT 5 -> js, iv OR jb -> jc, is OR it -> iu, ld 
                                                  //| OR le -> lf, NOT fc -> fd, NOT dm -> dn, bn OR by -> bz, aj AND al -> am, cd
                                                  //|  LSHIFT 15 -> ch, jp AND ka -> kc, ci OR ct -> cu, gv AND gx -> gy, de AND d
                                                  //| k -> dm, x RSHIFT 5 -> aa, et RSHIFT 2 -> eu, x RSHIFT 1 -> aq, ia OR ig -> 
                                                  //| ih, bk LSHIFT 1 -> ce, y OR ae -> af, NOT ca -> cb, e AND f -> h, ia AND ig 
                                                  //| -> ii, ck AND cl -> cn, NOT jh -> ji, z OR aa -> ab, 1 AND en -> eo, ib AND 
                                                  //| ic -> ie, NOT eh -> ei, 
                                                  //| Output exceeds cutoff limit.

  type Signal = Int
  type State = Map[String, Signal]
  def asUnsigned(i: Int): Signal = i.toChar.toInt //> asUnsigned: (i: Int)day7.day7.Signal

  def reduce(state: State, e: String): State = {
    (e match {
      case r"([0-9]+)$num -> ([a-z]+)$name"                   => Some(state.updated(name, num.toInt))
      case r"([a-z]+)$in -> ([a-z]+)$name"                    => for { s <- state.get(in) } yield state.updated(name, s)
      case r"([a-z]+)$in1 AND ([a-z]+)$in2 -> ([a-z]+)$out"   => for { s1 <- state.get(in1); s2 <- state.get(in2) } yield state.updated(out, asUnsigned(s1 & s2))
      case r"([0-9]+)$in1 AND ([a-z]+)$in2 -> ([a-z]+)$out"   => for { s2 <- state.get(in2) } yield state.updated(out, asUnsigned(in1.toInt & s2))
      case r"([a-z]+)$in1 OR ([a-z]+)$in2 -> ([a-z]+)$out"    => for { s1 <- state.get(in1); s2 <- state.get(in2) } yield state.updated(out, asUnsigned(s1 | s2))
      case r"([a-z]+)$in RSHIFT ([0-9]+)$num -> ([a-z]+)$out" => for { s1 <- state.get(in) } yield state.updated(out, asUnsigned(s1 >>> num.toInt))
      case r"([a-z]+)$in LSHIFT ([0-9]+)$num -> ([a-z]+)$out" => for { s1 <- state.get(in) } yield state.updated(out, asUnsigned(s1 << num.toInt))
      case r"NOT ([a-z]+)$in -> ([a-z]+)$out"                 => for { s1 <- state.get(in) } yield state.updated(out, asUnsigned(~s1))
    }).getOrElse(state)
  }                                               //> reduce: (state: day7.day7.State, e: String)day7.day7.State

  def update(state: State): State = {
    val updated = wires.foldLeft(state)(reduce)
    if (state != updated) update(updated) else updated
  }                                               //> update: (state: day7.day7.State)day7.day7.State

  update(Map())("a")                              //> res0: day7.day7.Signal = 14710
}