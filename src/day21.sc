object day21 {
  case class Item(name: String, cost: Int, damage: Int, armor: Int) {
    override def toString = name
  }

  val weapons: Set[Item] = Set(
    Item("Dagger", 8, 4, 0),
    Item("Shortsword", 10, 5, 0),
    Item("Warhammer", 25, 6, 0),
    Item("Longsword", 40, 7, 0),
    Item("Greataxe", 74, 8, 0))

  val armor: Set[Item] = Set(
    Item("No Armor", 0, 0, 0),
    Item("Leather", 13, 0, 1),
    Item("Chainmail", 31, 0, 2),
    Item("Splintmail", 53, 0, 3),
    Item("Bandedmail", 75, 0, 4),
    Item("Platemail", 102, 0, 5))

  val rings: Set[Item] = Set(
    Item("None 1", 0, 0, 0),
    Item("None 2", 0, 0, 0),
    Item("Damage +1", 25, 1, 0),
    Item("Damage +2", 50, 2, 0),
    Item("Damage +3", 100, 3, 0),
    Item("Defense +1", 20, 0, 1),
    Item("Defense +2", 40, 0, 2),
    Item("Defense +3", 80, 0, 3)
  )

  case class Hero(name: String, hp: Int, damage: Int, armor: Int) {
    def takeHit(hitDamage: Int): Option[Hero] = {
      val damageTaken = (hitDamage - armor) max 1
      if (damageTaken >= hp) None
      else Some(copy(hp=hp - damageTaken))
    }
    def don(item: Item): Hero = {
      copy(damage=damage + item.damage, armor=armor + item.armor)
    }
  }

  def attackerWins(attacker: Hero, defender: Hero): Boolean = {
    val fogOfWar = defender.takeHit(attacker.damage)
    fogOfWar match {
      case None => true
      case Some(survivor) => !attackerWins(survivor, attacker)
    }
  }

  val player = Hero("Player", 100, 0, 0)
  val boss = Hero("Boss", 109, 8, 2)

  val cheapestWin = (for {
    weapon <- weapons
    armor <- armor
    ring1 <- rings
    ring2 <- rings - ring1
    equippedPlayer = player.don(weapon).don(armor).don(ring1).don(ring2)
    if attackerWins(equippedPlayer, boss)
  } yield (weapon.cost + armor.cost + ring1.cost + ring2.cost, weapon, armor, ring1, ring2)).minBy{_._1}

  val mostExpensiveLoss = (for {
    weapon <- weapons
    armor <- armor
    ring1 <- rings
    ring2 <- rings - ring1
    playerDonned = player.don(weapon).don(armor).don(ring1).don(ring2)
    if !attackerWins(playerDonned, boss)
  } yield (weapon.cost + armor.cost + ring1.cost + ring2.cost, weapon, armor, ring1, ring2)).maxBy{_._1}

}