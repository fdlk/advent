object day22 {

  case class Player(hp: Int, mana: Int, armor: Int, manaSpent: Int) {
    def withArmor(newArmor: Int) = Player(hp, mana, newArmor, manaSpent)
    def takeHit(hitDamage: Int): Option[Player] = {
      val damageTaken = Math.max(hitDamage - armor, 1)
      if (damageTaken >= hp) None
      else Some(Player(hp - damageTaken, mana, armor, manaSpent))
    }
    def heal(healed: Int) = Player(hp + healed, mana, armor, manaSpent)
    def cast(cost: Int) = Player(hp, mana - cost, armor, manaSpent + cost)
    def recharge(buff: Int) = Player(hp, mana + buff, armor, manaSpent)
    override def toString = "Player has " + hp + " hit points, " + armor + " armor, " + mana + " mana"
  }

  case class Boss(hp: Int, damage: Int) {
    def takeDamage(damageTaken: Int): Option[Boss] = {
      if (damageTaken >= hp) None
      else Some(Boss(hp - damageTaken, damage))
    }
    override def toString = "Boss has " + hp + " hit points."
  }

  case class Spell(name: String, mana: Int)
  object Recharge extends Spell("Recharge", 229)
  object MagicMissile extends Spell("Magic Missile", 53)
  object Drain extends Spell("Drain", 73)
  object Shield extends Spell("Shield", 113)
  object Poison extends Spell("Poison", 173)
  case class SpellEffect(spell: Spell, turnsLeft: Int) {
    def countDown = SpellEffect(spell, turnsLeft - 1)
  }

  abstract class Status {
    def playerCastSpell(spell: Spell) = this
   // def playerTakeHitPart2 = this
    def bossHitPlayer = this
    def isWon: Option[Int] = None
    def isNotLost: Boolean = false
  }

  case class PlayerWon(manaSpent: Int) extends Status {
    println(manaSpent)
    override def isWon = Some(manaSpent)
    override def isNotLost = true
  }

  case class PlayerLost() extends Status
  case class Battle(player: Player, boss: Boss, spellEffects: List[SpellEffect]) extends Status {
    override def isNotLost = player.manaSpent < 1390 // prune branches that spend too much mana
    override def toString = player.toString + "\n" + boss.toString + "\n" + spellEffects
    def effectIsRunning(spell: Spell): Boolean = spellEffects.exists(eff => (eff.spell == spell) && (eff.turnsLeft>0))
    def withSpellEffect(drainedPlayer: Player, spellEffect: SpellEffect): Battle =
      Battle(drainedPlayer, boss, spellEffect :: spellEffects)
    override def playerCastSpell(spell: Spell): Status = {
      if (player.mana < spell.mana) PlayerLost()
      else if (effectIsRunning(spell)) PlayerLost()
      else {
        val drainedPlayer = player.cast(spell.mana)
        spell match {
          case MagicMissile => {
            boss.takeDamage(4) match {
              case None => PlayerWon(drainedPlayer.manaSpent)
              case Some(survivingBoss) => Battle(drainedPlayer, survivingBoss, spellEffects)
            }
          }
          case Drain => {
            boss.takeDamage(2) match {
              case None => PlayerWon(drainedPlayer.manaSpent)
              case Some(survivingBoss) => Battle(drainedPlayer.heal(2), survivingBoss, spellEffects)
            }
          }
          case Shield => withSpellEffect(drainedPlayer, SpellEffect(Shield, 6))
          case Poison => withSpellEffect(drainedPlayer, SpellEffect(Poison, 6))
          case Recharge => withSpellEffect(drainedPlayer, SpellEffect(Recharge, 5))
        }
      }
    }

    //    override def playerTakeHitPart2: Status = {
    //      player.takeHit(1) match {
    //        case None => PlayerLost()
    //        case Some(nickedPlayer) => Battle(nickedPlayer, boss, spellEffects)
    //      }
    //    }
    override def bossHitPlayer: Status = {
      player.takeHit(boss.damage) match {
        case None => PlayerLost()
        case Some(survivingPlayer) => Battle(survivingPlayer, boss, spellEffects)
      }
    }
  }

  def countedDownSpellEffects(spellEffects: List[SpellEffect]): List[SpellEffect] =
    for (effect <- spellEffects if effect.turnsLeft > 0)
      yield effect.countDown

  def resolveSpellEffects(status: Status): Status = {
    status match {
      case Battle(player, boss, spellEffects) => {
        val countedDown: List[SpellEffect] = countedDownSpellEffects(spellEffects)
        val initialState = Battle(player.withArmor(0), boss, countedDown).asInstanceOf[Status]
        countedDown.foldLeft(initialState)(resolveSpellEffect)
      }
      case x => x
    }

  }

  def resolveSpellEffect(status: Status, spellEffect: SpellEffect): Status = status match {
    case Battle(player, boss, spellEffects) => {
      spellEffect.spell match {
        case Shield => Battle(player.withArmor(7), boss, spellEffects)
        case Poison => boss.takeDamage(3) match {
          case None => PlayerWon(player.manaSpent)
          case Some(poisonedBoss) => Battle(player, poisonedBoss, spellEffects)
        }
        case Recharge => Battle(player.recharge(101), boss, spellEffects)
      }
    }
    case x => x
  }
  val initialGameState: Status = Battle(Player(50, 500, 0, 0), Boss(58, 9), List())
  def reduceGameState(status: Status, spell: Spell): Status = {
    val step1: Status = resolveSpellEffects(status)
    val step2: Status = step1.playerCastSpell(spell)
    //.playerTakeHitPart2
    val step3: Status = resolveSpellEffects(step2)
    val step4: Status = step3.bossHitPlayer
    step4
  }
  case class History(spells: List[Spell], status: Status) {
    def update: List[History] = {
      if (status.isWon.isDefined) {
        List(this)
      } else for {
        spell <- List(Recharge, MagicMissile, Poison, Shield, Drain)
        updatedGameState = reduceGameState(status, spell)
        updatedHistory = History(spell :: spells, updatedGameState)
        if updatedHistory.status.isNotLost
      } yield updatedHistory
    }
  }
  def oneMoreSpell(histories: List[History], i: Int): List[History] = {
    println(i, histories.size)
    histories.flatMap(_.update)
  }
  val solutions: List[History] = (1 to 30).foldLeft(List(History(Nil, initialGameState)))(oneMoreSpell)
  val minManaSpent = (for {
    s <- solutions
    manaSpent <- s.status.isWon
  } yield (manaSpent, s.spells.reverse)).minBy(_._1)
  // Sanity check:
  //val solution:List[Spell] = List(Poison, Recharge, MagicMissile, Poison, Recharge, Shield, Poison, Drain, MagicMissile)
  //solution.foldLeft(initialGameState)(reduceGameState)
}