object day15 {
  val pattern = """(\w+): capacity (-?\d+), durability (-?\d+), flavor (-?\d+), texture (-?\d+), calories (-?\d+)""".r

  case class Ingredient(name: String, capacity: Int, durability: Int, flavor: Int, texture: Int, calories: Int)

  val ingredients = common.loadPackets(List("day15", "day15.txt")).map {
    case pattern(name, capacity, durability, flavor, texture, calories) =>
      Ingredient(name, capacity.toInt, durability.toInt, flavor.toInt, texture.toInt, calories.toInt)
  }

  def score(recipe: List[(Int, Ingredient)]): Int = {
    val capacity = (for {
      (amount, ingredient) <- recipe
    } yield ingredient.capacity * amount).sum

    val durability = (for {
      (amount, ingredient) <- recipe
    } yield ingredient.durability * amount).sum

    val flavor = (for {
      (amount, ingredient) <- recipe
    } yield ingredient.flavor * amount).sum

    val texture = (for {
      (amount, ingredient) <- recipe
    } yield ingredient.texture * amount).sum

    Math.max(capacity, 0) * Math.max(durability, 0) * Math.max(flavor, 0) * Math.max(texture, 0)
  }

  def combinations(ingredients: List[Ingredient], leftover: Int): List[List[(Int, Ingredient)]] = {
    ingredients match {
      case Nil => if( leftover == 0 ) List(Nil) else Nil
      case i :: is => (for {
        amount <- 0 to leftover
        combination <- combinations(is, leftover - amount)
        combination2: List[(Int, Ingredient)] = (amount, i) :: combination
      } yield combination2).toList
    }
  }
  combinations(ingredients, 100).map(score).max

  def combinationsWithCalories(ingredients: List[Ingredient], leftover: Int, caloriesLeft: Int): List[List[(Int, Ingredient)]] = {
    ingredients match {
      case Nil => if (caloriesLeft == 0 && leftover == 0 ) List(Nil) else Nil
      case i :: is => (for {
        amount <- 0 to leftover if caloriesLeft - amount * i.calories >= 0
        combination <- combinationsWithCalories(is, leftover - amount, caloriesLeft - amount * i.calories)
        combination2: List[(Int, Ingredient)] = (amount, i) :: combination
      } yield combination2).toList
    }
  }
  combinationsWithCalories(ingredients, 100, 500).map(score).max
}