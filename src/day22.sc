object day22 {

  case class Player(hp: Int, mana: Int, armor: Int, spellsCast: List[Spell]) {
    def withArmor(newArmor: Int) = Player(hp, mana, newArmor, spellsCast)
    def takeHit(hitDamage: Int): Option[Player] = {
      val damageTaken = Math.max(hitDamage - armor, 1)
      if (damageTaken >= hp) None
      else Some(Player(hp - damageTaken, mana, armor, spellsCast))
    }
    def heal(healed: Int) = Player(hp + healed, mana, armor, spellsCast)
    def cast(spell:Spell) = Player(hp, mana - spell.mana, armor, spellsCast ::: List(spell))
    def recharge(buff: Int) = Player(hp, mana + buff, armor, spellsCast)
    def manaSpent: Int = spellsCast.map(_.mana).sum
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
    def countDown:Option[SpellEffect] =
      if(turnsLeft == 1) None
      else Some(SpellEffect(spell, turnsLeft - 1))
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
      playerTakeHitPart2.
        resolveSpellEffects
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

  case class PlayerWon(player:Player) extends GameState {
    println(bossIsDead)
    override def bossIsDead = Some(player.spellsCast.map(_.mana).sum)
    override def lookingGood = true
  }
  case class PlayerLost(player:Player) extends GameState
  case class Battle(player: Player, boss: Boss, spellEffects: List[SpellEffect], hardmode:Boolean) extends GameState {
    override def lookingGood = player.manaSpent < 1390 // prune branches that spend too much mana
    override def toString = player.toString + "\n" + boss.toString + "\n" + spellEffects

    def effectIsRunning(spell: Spell): Boolean = spellEffects.exists(_.spell == spell)
    override def countDownSpellEffects: Battle = Battle(player, boss,
      for (effect <- spellEffects; countedDownEffect <- effect.countDown) yield countedDownEffect, hardmode)
    override def resolveSpellEffect(spellEffect: SpellEffect): GameState = spellEffect.spell match {
      case Shield => Battle(player.withArmor(7), boss, spellEffects, hardmode)
      case Poison => boss.takeDamage(3) match {
        case None => PlayerWon(player)
        case Some(poisonedBoss) => Battle(player, poisonedBoss, spellEffects, hardmode)
      }
      case Recharge => Battle(player.recharge(101), boss, spellEffects, hardmode)
    }
    override def resolveSpellEffects: GameState = {
      val initialState = Battle(player.withArmor(0), boss, spellEffects, hardmode).asInstanceOf[GameState]
      val result:GameState = spellEffects.foldLeft(initialState)((status, spell)=>status.resolveSpellEffect(spell))
      result.countDownSpellEffects
    }
    override def playerCastSpell(spell: Spell): GameState = {
      if (player.mana < spell.mana) PlayerLost(player)
      else if (effectIsRunning(spell)) PlayerLost(player)
      else {
        val drainedPlayer = player.cast(spell)
        spell match {
          case MagicMissile => {
            boss.takeDamage(4) match {
              case None => PlayerWon(drainedPlayer)
              case Some(survivingBoss) => Battle(drainedPlayer, survivingBoss, spellEffects, hardmode)
            }
          }
          case Drain => {
            boss.takeDamage(2) match {
              case None => PlayerWon(drainedPlayer)
              case Some(survivingBoss) => Battle(drainedPlayer.heal(2), survivingBoss, spellEffects, hardmode)
            }
          }
          case Shield => Battle(drainedPlayer, boss, SpellEffect(Shield, 6) :: spellEffects, hardmode)
          case Poison => Battle(drainedPlayer, boss, SpellEffect(Poison, 6) :: spellEffects, hardmode)
          case Recharge => Battle(drainedPlayer, boss, SpellEffect(Recharge, 5) :: spellEffects, hardmode)
        }
      }
    }
    override def playerTakeHitPart2: GameState = {
      if(!hardmode) this else player.takeHit(1) match {
        case None => PlayerLost(player)
        case Some(nickedPlayer) => Battle(nickedPlayer, boss, spellEffects, hardmode)
      }
    }
    override def bossHitPlayer: GameState = {
      player.takeHit(boss.damage) match {
        case None => PlayerLost(player)
        case Some(survivingPlayer) => Battle(survivingPlayer, boss, spellEffects, hardmode)
      }
    }
  }

  def nextTurn(gameStates: List[GameState], i: Int): List[GameState] = {
    println(i, gameStates.size)
    gameStates.flatMap(_.nextTurn)
  }
  val hardmode: Boolean = false // part 1 / 2
  val initialGameState: GameState = Battle(Player(50, 500, 0, Nil), Boss(58, 9), Nil, hardmode)
  val solutions: List[GameState] = (1 to 30).foldLeft(List(initialGameState))(nextTurn)
  val minManaSpent = (for {
    s <- solutions
    manaSpent <- s.bossIsDead
  } yield manaSpent).min

  // Sanity check:
  val solution:List[Spell] = List(Poison, Recharge, MagicMissile, Poison, Recharge, Shield, Poison, Drain, MagicMissile)
  solution.foldLeft(initialGameState)(_.nextTurn(_))
}