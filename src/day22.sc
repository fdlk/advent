object day22 {

  case class Spell(name: String, mana: Int)
  case class SpellEffect(spell: Spell, turnsLeft: Int)
  case class Player(hp: Int, mana: Int, armor: Int, manaSpent: Int) {
    def withArmor(newArmor: Int) = Player(hp, mana, newArmor, manaSpent)

    def takeHit(hitDamage: Int): Option[Player] = {
      val damageTaken = Math.max(hitDamage - armor, 1)
      if (damageTaken >= hp) None
      else Some(Player(hp - damageTaken, mana, armor, manaSpent))
    }

    def heal(healed: Int) = Player(hp + healed, mana, armor, manaSpent)

    def cast(cost: Int) = Player(hp, mana - cost, armor, manaSpent + mana)

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

  class GameState
  case class Battling(player: Player, boss: Boss, spellEffects: List[SpellEffect]) extends GameState {
    override def toString = player.toString + "\n" + boss.toString + "\n" + spellEffects
  }

  case class PlayerLost() extends GameState
  case class PlayerWon(manaSpent: Int) extends GameState{
    println(manaSpent)
  }
  case class PlayerOOM() extends GameState
  case class EffectStillRunning() extends GameState
  val fizzle = Spell("Fizzle", 0)
  val recharge = Spell("Recharge", 229)
  val magicMissile = Spell("Magic Missile", 53)
  val drain = Spell("Drain", 73)
  val shield = Spell("Shield", 113)
  val poison = Spell("Poison", 173)
  val spells: List[Spell] = List(fizzle, recharge, magicMissile, poison, shield, drain)
  def resolveSpellEffect(playerAndBoss: (Player, Option[Boss]), spellEffect: SpellEffect): (Player, Option[Boss]) =
    playerAndBoss match {
      case (player, Some(boss)) => spellEffect match {
        case s: SpellEffect if s.spell == shield => (player.withArmor(7), Some(boss))
        case s: SpellEffect if s.spell == poison => (player, boss.takeDamage(3))
        case s: SpellEffect if s.spell == recharge => (player.recharge(101), Some(boss))
      }
      case _ => playerAndBoss
    }

  def countDownSpellEffects(effects: List[SpellEffect]): List[SpellEffect] = for {
    effect <- effects if effect.turnsLeft > 1
  } yield SpellEffect(effect.spell, effect.turnsLeft - 1)

  def resolveSpellEffects(gameState: Battling): GameState = {
    val initial: (Player, Option[Boss]) = (gameState.player.withArmor(0), Some(gameState.boss))
    val (buffedPlayer, fogOfWar) = gameState.spellEffects.foldLeft(initial)(resolveSpellEffect)
    fogOfWar match {
      case None => PlayerWon(buffedPlayer.manaSpent)
      case Some(bossSurvived) => {
        Battling(buffedPlayer, bossSurvived, countDownSpellEffects(gameState.spellEffects))
      }
    }
  }

  def effectIsRunning(spell: Spell, gameState: Battling): Boolean = {
    gameState.spellEffects.exists({
      _.spell.name == spell.name
    })
  }

  def castSpell(gameState: Battling, spell: Spell): GameState = {
    if (gameState.player.mana < spell.mana) PlayerOOM()
    else if (effectIsRunning(spell, gameState)) EffectStillRunning()
    else {
      val drainedPlayer = gameState.player.cast(spell.mana)
      spell.name match {
        case magicMissile.name => {
          val fogOfWar = gameState.boss.takeDamage(4)
          fogOfWar match {
            case None => PlayerWon(drainedPlayer.manaSpent)
            case Some(survivingBoss) => Battling(drainedPlayer, survivingBoss, gameState.spellEffects)
          }
        }
        case drain.name => {
          val fogOfWar = gameState.boss.takeDamage(2)
          fogOfWar match {
            case None => PlayerWon(drainedPlayer.manaSpent)
            case Some(survivingBoss) => Battling(drainedPlayer.heal(2), survivingBoss, gameState.spellEffects)
          }
        }
        case shield.name => {
          Battling(drainedPlayer, gameState.boss, SpellEffect(shield, 6) :: gameState.spellEffects)
        }
        case poison.name => {
          Battling(drainedPlayer, gameState.boss, SpellEffect(poison, 6) :: gameState.spellEffects)
        }
        case recharge.name => {
          Battling(drainedPlayer, gameState.boss, SpellEffect(recharge, 5) :: gameState.spellEffects)
        }
        case fizzle.name => {
          gameState
        }
      }
    }
  }

  def reduceGameState(gameState: GameState, spell: Spell): GameState = gameState match {
    case b: Battling => {
      resolveSpellEffects(b) match {
        case b: Battling => {
          castSpell(b, spell) match {
            case b: Battling => resolveSpellEffects(b) match {
              case Battling(player, boss, spellEffects) => {
                player.takeHit(boss.damage) match {
                  case None => PlayerLost()
                  case Some(survivingPlayer) => Battling(survivingPlayer, boss, spellEffects)
                }
              }
              case x => x
            }
            case x => x
          }
        }
        case x => x
      }
    }
    case x => x
  }


  val initialGameState: GameState = Battling(Player(50, 500, 0, 0), Boss(58, 9), List())
  def resolve(history: List[Spell]): GameState = {
    history.foldLeft(initialGameState)(reduceGameState)
  }
  resolve(List(poison, poison))

  case class History(spells: List[Spell], gameState: GameState) {
    def isWon: Option[Int] = gameState match {
      case PlayerWon(manaSpent) => Some(manaSpent)
      case _ => None
    }

    def isNotLost: Boolean = gameState match {
      case PlayerWon(manaSpent) => true
      case Battling(player, _,_) => player.manaSpent < 2400
      case _ => false
    }
  }

  def nSpells(n: Int): List[History] = {
    println(n)
    if (n == 0) {
      List(History(Nil,initialGameState))
    }
    else {
      val result = for {
        history <- nSpells(n - 1)
        s <- spells
        updatedGameState = reduceGameState(history.gameState, s)
        updatedHistory = History(s::history.spells, updatedGameState)
        if updatedHistory.isNotLost
      } yield updatedHistory
      println(n, result.size)
      result
    }
  }
  (for {
    history <- nSpells(10)
    manaSpent <- history.isWon
  } yield (manaSpent, history)).minBy(_._1)
}