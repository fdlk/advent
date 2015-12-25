object day22 {

  case class Player(hp: Int, mana: Int, armor: Int, spellsCast: List[Spell]) {
    def withArmor(newArmor: Int) = copy(armor = newArmor)

    def takeHit(hitDamage: Int): Option[Player] = {
      val damageTaken = (hitDamage - armor) max 1
      if (damageTaken >= hp) None
      else Some(copy(hp = hp - damageTaken))
    }

    def heal(healed: Int) = copy(hp = hp + healed)

    def cast(spell: Spell) = copy(mana = mana - spell.mana, spellsCast = spellsCast ::: List(spell))

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
    def playerCastSpell(spell: Spell): GameState = this

    def resolveSpellEffect(spellEffect: SpellEffect): GameState = this

    def resolveSpellEffects: GameState = this

    def countDownSpellEffects: GameState = this

    def playerTakeHitPart2 = this

    def bossHitPlayer = this

    def bossIsDead: Option[Int] = None

    def lookingGood: Boolean = false

    def nextTurn(spell: Spell): GameState = {
      playerTakeHitPart2
        .resolveSpellEffects
        .playerCastSpell(spell)
        .resolveSpellEffects
        .bossHitPlayer
    }

    def nextTurn: List[GameState] = {
      if (bossIsDead.isDefined) {
        List(this)
      } else for {
        spell <- List(Recharge, MagicMissile, Poison, Shield, Drain)
        updatedGameState = nextTurn(spell) if updatedGameState.lookingGood
      } yield updatedGameState
    }
  }

  case class PlayerWon(player: Player) extends GameState {
    println(bossIsDead)

    override def bossIsDead = Some(player.spellsCast.map(_.mana).sum)

    override def lookingGood = true
  }

  case class PlayerLost(player: Player) extends GameState
  case class Battle(player: Player, boss: Boss, spellEffects: List[SpellEffect], hardmode: Boolean) extends GameState {
    override def lookingGood = player.manaSpent < 1400

    // prune branches that spend too much mana
    override def toString = player.toString + "\n" + boss.toString + "\n" + spellEffects

    def effectIsRunning(spell: Spell): Boolean = spellEffects.exists(_.spell == spell)

    override def countDownSpellEffects: Battle = copy(spellEffects =
      for (effect <- spellEffects; countedDownEffect <- effect.countDown) yield countedDownEffect)

    override def resolveSpellEffect(spellEffect: SpellEffect): GameState = spellEffect.spell match {
      case Shield => copy(player = player.withArmor(7))
      case Poison => boss.takeDamage(3) match {
        case None => PlayerWon(player)
        case Some(poisonedBoss) => copy(boss = poisonedBoss)
      }
      case Recharge => copy(player = player.recharge(101))
    }

    override def resolveSpellEffects: GameState = {
      val initialState = copy(player = player.withArmor(0)).asInstanceOf[GameState]
      val result: GameState = spellEffects.foldLeft(initialState)((status, spell) => status.resolveSpellEffect(spell))
      result.countDownSpellEffects
    }

    override def playerCastSpell(spell: Spell): GameState = {
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

    override def playerTakeHitPart2: GameState = {
      if (!hardmode) this
      else player.takeHit(1) match {
        case None => PlayerLost(player)
        case Some(nickedPlayer) => copy(player = nickedPlayer)
      }
    }

    override def bossHitPlayer: GameState = {
      player.takeHit(boss.damage) match {
        case None => PlayerLost(player)
        case Some(survivingPlayer) => copy(player = survivingPlayer)
      }
    }
  }

  def nextTurn(gameStates: List[GameState]): List[GameState] = {
    println(gameStates.size)
    gameStates.flatMap(_.nextTurn)
  }

  // part 1 / 2
  val hardmode: Boolean = true
  val initialGameState: GameState = Battle(Player(50, 500, 0, Nil), Boss(58, 9), Nil, hardmode)
  val solutions: List[GameState] = Stream.iterate(List(initialGameState))(nextTurn).drop(30).head
  val minManaSpent = (for {
    s <- solutions
    manaSpent <- s.bossIsDead
  } yield manaSpent).min
  // Sanity check:
  val solution: List[Spell] = List(Poison, Recharge, MagicMissile, Poison, Recharge, Shield, Poison, Drain, MagicMissile)
  solution.foldLeft(initialGameState)(_.nextTurn(_))
}