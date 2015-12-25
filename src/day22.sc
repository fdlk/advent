object day22 {

  case class Player(hp: Int, mana: Int, armor: Int, spellsCast: List[Spell]) {
    def withArmor(newArmor: Int) = copy(armor = newArmor)

    def takeHit(hitDamage: Int): Option[Player] = {
      val damageTaken = (hitDamage - armor) max 1
      if (damageTaken >= hp) None
      else Some(copy(hp = hp - damageTaken))
    }

    def heal(healed: Int) = copy(hp = hp + healed)

    def cast(spell: Spell) = copy(mana = mana - spell.mana, spellsCast = spellsCast :+ spell)

    def recharge(buff: Int) = copy(mana = mana + buff)

    def manaSpent: Int = spellsCast.map(_.mana).sum

    override def toString = "Player has " + hp + " hit points, " + armor + " armor, " + mana + " mana"
  }

  case class Boss(hp: Int, damage: Int) {
    def takeDamage(damageTaken: Int): Option[Boss] = {
      if (damageTaken >= hp) None
      else Some(copy(hp = hp - damageTaken))
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
    def countDown: Option[SpellEffect] =
      if (turnsLeft == 1) None
      else Some(copy(turnsLeft = turnsLeft - 1))
  }

  abstract class GameState {
    def bossIsDead: Option[Int] = None

    def flatMap(f: Battle => GameState): GameState = this match {
      case b: Battle => f(b)
      case _ => this
    }

    def map(f: Battle => Battle): GameState = flatMap(f)

    def nextTurn: List[GameState] = Nil
  }

  case class PlayerWon(player: Player) extends GameState {
    println(bossIsDead)
    override def bossIsDead = Some(player.spellsCast.map(_.mana).sum)
    override def nextTurn = List(this)
  }

  case class PlayerLost(player: Player) extends GameState
  case class ManaGuzzler(player: Player) extends GameState
  case class Battle(player: Player, boss: Boss, spellEffects: List[SpellEffect], hardmode: Boolean) extends GameState {
    override def toString = player.toString + "\n" + boss.toString + "\n" + spellEffects
    def effectIsRunning(spell: Spell): Boolean = spellEffects.exists(_.spell == spell)
    def countDownSpellEffects: Battle = copy(spellEffects =
      for (effect <- spellEffects; countedDownEffect <- effect.countDown) yield countedDownEffect)
    def resolveSpellEffect(spellEffect: SpellEffect): GameState = spellEffect.spell match {
      case Shield => copy(player = player.withArmor(7))
      case Poison => boss.takeDamage(3) match {
        case None => PlayerWon(player)
        case Some(poisonedBoss) => copy(boss = poisonedBoss)
      }
      case Recharge => copy(player = player.recharge(101))
    }

    def resolveSpellEffects: GameState = {
      val initialState = copy(player = player.withArmor(0)).asInstanceOf[GameState]
      for {
        result <- spellEffects.foldLeft(initialState)(
          (status, spell) => status.flatMap(_.resolveSpellEffect(spell))
        )
      } yield result.countDownSpellEffects
    }

    def playerCastSpell(spell: Spell): GameState = {
      if (player.mana < spell.mana) PlayerLost(player)
      else if (effectIsRunning(spell)) PlayerLost(player)
      else {
        val drainedPlayer = player.cast(spell)
        spell match {
          case MagicMissile => boss.takeDamage(4) match {
            case None => PlayerWon(drainedPlayer)
            case Some(survivingBoss) => copy(player = drainedPlayer, boss = survivingBoss)
          }
          case Drain => boss.takeDamage(2) match {
            case None => PlayerWon(drainedPlayer)
            case Some(survivingBoss) => copy(player = drainedPlayer.heal(2), boss = survivingBoss)
          }
          case Shield => copy(player = drainedPlayer, spellEffects = SpellEffect(Shield, 6) :: spellEffects)
          case Poison => copy(player = drainedPlayer, spellEffects = SpellEffect(Poison, 6) :: spellEffects)
          case Recharge => copy(player = drainedPlayer, spellEffects = SpellEffect(Recharge, 5) :: spellEffects)
        }
      }
    }

    def playerTakeHitPart2: GameState = {
      if (!hardmode) this
      else player.takeHit(1) match {
        case None => PlayerLost(player)
        case Some(nickedPlayer) => copy(player = nickedPlayer)
      }
    }

    def bossHitPlayer: GameState = {
      player.takeHit(boss.damage) match {
        case None => PlayerLost(player)
        case Some(survivingPlayer) => copy(player = survivingPlayer)
      }
    }

    def checkManaSpent: GameState = {
      if (player.manaSpent < 1300) this else ManaGuzzler(player)
    }

    def nextTurn(spell: Spell): GameState = {
      // During each step, the battle may end and the next battle steps become irrelevant
      // here's where we solve that
      for {
        step1 <- playerTakeHitPart2
        step2 <- step1.resolveSpellEffects
        step3 <- step2.playerCastSpell(spell)
        step4 <- step3.resolveSpellEffects
        step5 <- step4.bossHitPlayer
        step6 <- step5.checkManaSpent
      } yield step6
    }

    override def nextTurn : List[GameState] = {
      for {
        spell <- List(Recharge, MagicMissile, Poison, Shield, Drain)
        updatedGameState = flatMap(_.nextTurn(spell))
      } yield updatedGameState
    }
  }

  def nextTurn(gameStates: List[GameState]): List[GameState] = {
    println(gameStates.size)
    gameStates.flatMap(_.nextTurn)
  }
  // part 1 / 2
  val hardmode: Boolean = false
  val initialGameState: GameState = Battle(Player(50, 500, 0, Nil), Boss(58, 9), Nil, hardmode)
  val solutions: List[GameState] = Stream.iterate(List(initialGameState))(nextTurn).drop(30).head
  val minManaSpent = (for {
    s <- solutions
    manaSpent <- s.bossIsDead
  } yield manaSpent).min
  // Sanity check:
  val solution: List[Spell] = List(Poison, Recharge, MagicMissile, Poison, Recharge, Shield, Poison, Drain, MagicMissile)
  solution.foldLeft(initialGameState)((state, spell) => state.flatMap(_.nextTurn(spell)))
}