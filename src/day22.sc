object day22 {
  case class SpellEffect(spell: Spell, turnsLeft: Int)

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

  class GameState

  case class Battling(player: Player, boss: Boss, spellEffects: List[SpellEffect]) extends GameState {
    override def toString = player.toString + "\n" + boss.toString + "\n" + spellEffects
  }

  case class PlayerLost() extends GameState
  case class PlayerWon(manaSpent: Int) extends GameState {
    println(manaSpent)
  }

  case class Spell(name: String, mana: Int){
    def effectIsRunning(gameState: Battling): Boolean = gameState.spellEffects.exists(_.spell == this)
  }
  object Recharge extends Spell("Recharge", 229)
  object MagicMissile extends Spell("Magic Missile", 53)
  object Drain extends Spell("Drain", 73)
  object Shield extends Spell("Shield", 113)
  object Poison extends Spell("Poison", 173)
  val spells: List[Spell] = List(Recharge, MagicMissile, Poison, Shield, Drain)

  def resolveSpellEffect(player: Player, boss: Boss, spell: Spell): (Player, Option[Boss]) = spell match {
    case Shield => (player.withArmor(7), Some(boss))
    case Poison => (player, boss.takeDamage(3))
    case Recharge => (player.recharge(101), Some(boss))
  }

  def countDownSpellEffects(effects: List[SpellEffect]): List[SpellEffect] = for {
    effect <- effects if effect.turnsLeft > 1
  } yield SpellEffect(effect.spell, effect.turnsLeft - 1)

  def resolveSpellEffects(gameState: Battling): GameState = {
    val initial: (Player, Option[Boss]) = (gameState.player.withArmor(0), Some(gameState.boss))
    val (buffedPlayer, fogOfWar) = gameState.spellEffects.foldLeft(initial)({
      case ((player, None), _) => (player, None);
      case ((player, Some(boss)), SpellEffect(spell, _)) => resolveSpellEffect(player, boss, spell)
    })
    fogOfWar match {
      case None => PlayerWon(buffedPlayer.manaSpent)
      case Some(bossSurvived) => {
        Battling(buffedPlayer, bossSurvived, countDownSpellEffects(gameState.spellEffects))
      }
    }
  }

  def castSpell(gameState: Battling, spell: Spell): GameState = {
    if (gameState.player.mana < spell.mana) PlayerLost()
    else if (spell.effectIsRunning(gameState)) PlayerLost()
    else {
      val drainedPlayer = gameState.player.cast(spell.mana)
      spell match {
        case MagicMissile => {
          val fogOfWar = gameState.boss.takeDamage(4)
          fogOfWar match {
            case None => PlayerWon(drainedPlayer.manaSpent)
            case Some(survivingBoss) => Battling(drainedPlayer, survivingBoss, gameState.spellEffects)
          }
        }
        case Drain => {
          val fogOfWar = gameState.boss.takeDamage(2)
          fogOfWar match {
            case None => PlayerWon(drainedPlayer.manaSpent)
            case Some(survivingBoss) => Battling(drainedPlayer.heal(2), survivingBoss, gameState.spellEffects)
          }
        }
        case Shield => {
          Battling(drainedPlayer, gameState.boss, SpellEffect(Shield, 6) :: gameState.spellEffects)
        }
        case Poison => {
          Battling(drainedPlayer, gameState.boss, SpellEffect(Poison, 6) :: gameState.spellEffects)
        }
        case Recharge => {
          Battling(drainedPlayer, gameState.boss, SpellEffect(Recharge, 5) :: gameState.spellEffects)
        }
      }
    }
  }

  val part12 = 0
  def reduceGameState(gameState: GameState, spell: Spell): GameState = gameState match {
    case b: Battling => {
      resolveSpellEffects(b) match {
        case b: Battling => {
          castSpell(b, spell) match {
            case Battling(player, boss, spellEffects) => player.takeHit(part12) match {
              case None => PlayerLost()
              case Some(hitPlayer) => resolveSpellEffects(Battling(hitPlayer, boss, spellEffects)) match {
                case Battling(player, boss, spellEffects) => {
                  player.takeHit(boss.damage) match {
                    case None => PlayerLost()
                    case Some(survivingPlayer) => Battling(survivingPlayer, boss, spellEffects)
                  }
                }
                case x => x
              }
            }
            case x => x
          }
        }
        case x => x
      }
    }
    case x => x
  }

  case class History(spells: List[Spell], gameState: GameState) {
    def isWon: Option[Int] = gameState match {
      case PlayerWon(manaSpent) => Some(manaSpent)
      case _ => None
    }

    def isNotLost: Boolean = gameState match {
      case PlayerWon(manaSpent) => true
      case Battling(player, _, _) => player.manaSpent < 1500
      case _ => false
    }
  }

  def updateHistory(history: History): List[History] = {
    if (history.isWon.isDefined) {
      List(history)
    } else for {
      s <- spells
      updatedGameState = reduceGameState(history.gameState, s)
      updatedHistory = History(s :: history.spells, updatedGameState)
      if updatedHistory.isNotLost
    } yield updatedHistory
  }

  def oneMoreSpell(histories: List[History], i: Int): List[History] = {
    println(i, histories.size)
    histories.flatMap(updateHistory)
  }

  val initialGameState: GameState = Battling(Player(50, 500, 0, 0), Boss(58, 9), List())
  val solutions: List[History] = (1 to 35).foldLeft(List(History(Nil, initialGameState)))(oneMoreSpell)
  val minManaSpent = (for {
    s <- solutions
    manaSpent <- s.isWon
  } yield manaSpent).min
}