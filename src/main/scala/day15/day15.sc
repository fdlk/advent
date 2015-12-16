package day15

object day15 {
  val pattern = """(\w+): capacity (-?\d+), durability (-?\d+), flavor (-?\d+), texture (-?\d+), calories (-?\d+)""".r
                                                  //> pattern  : scala.util.matching.Regex = (\w+): capacity (-?\d+), durability (
                                                  //| -?\d+), flavor (-?\d+), texture (-?\d+), calories (-?\d+)

  case class Ingredient(name: String, capacity: Int, durability: Int, flavor: Int, texture: Int, calories: Int)

  val ingredients = common.loadPackets(List("day15", "day15.txt")).map {
    case pattern(name, capacity, durability, flavor, texture, calories) =>
      Ingredient(name, capacity.toInt, durability.toInt, flavor.toInt, texture.toInt, calories.toInt)
  }                                               //> ingredients  : List[day15.day15.Ingredient] = List(Ingredient(Frosting,4,-2,
                                                  //| 0,0,5), Ingredient(Candy,0,5,-1,0,8), Ingredient(Butterscotch,-1,0,5,0,6), I
                                                  //| ngredient(Sugar,0,0,-2,2,1))

  def score(recipe: List[(Int, Ingredient)]): Int = {
    val capacity = (for {
      (amount, ingredient) <- recipe
    } yield ingredient.capacity * amount).toList.sum

    val durability = (for {
      (amount, ingredient) <- recipe
    } yield ingredient.durability * amount).toList.sum

    val flavor = (for {
      (amount, ingredient) <- recipe
    } yield ingredient.flavor * amount).toList.sum

    val texture = (for {
      (amount, ingredient) <- recipe
    } yield ingredient.texture * amount).toList.sum

    Math.max(capacity, 0) * Math.max(durability, 0) * Math.max(flavor, 0) * Math.max(texture, 0)
  }                                               //> score: (recipe: List[(Int, day15.day15.Ingredient)])Int

  def combinations(ingredients: List[Ingredient], leftover: Int): List[List[(Int, Ingredient)]] = {
    ingredients match {
      case Nil => if( leftover == 0 ) List(Nil) else Nil
      case i :: is => (for {
        amount <- 0 to leftover
        combination <- combinations(is, leftover - amount)
        val combination2: List[(Int, Ingredient)] = (amount, i) :: combination
      } yield combination2).toList
    }
  }                                               //> combinations: (ingredients: List[day15.day15.Ingredient], leftover: Int)Lis
                                                  //| t[List[(Int, day15.day15.Ingredient)]]
  combinations(ingredients, 100).map(score).max   //> res0: Int = 18965440

  def combinationsWithCalories(ingredients: List[Ingredient], leftover: Int, caloriesLeft: Int): List[List[(Int, Ingredient)]] = {
    ingredients match {
      case Nil => if (caloriesLeft == 0 && leftover == 0 ) List(Nil) else Nil
      case i :: is => (for {
        amount <- 0 to leftover if caloriesLeft - amount * i.calories >= 0
        combination <- combinationsWithCalories(is, leftover - amount, caloriesLeft - amount * i.calories)
        combination2: List[(Int, Ingredient)] = (amount, i) :: combination
      } yield combination2).toList
    }
  }                                               //> combinationsWithCalories: (ingredients: List[day15.day15.Ingredient], lefto
                                                  //| ver: Int, caloriesLeft: Int)List[List[(Int, day15.day15.Ingredient)]]
  combinationsWithCalories(ingredients, 100, 500).map(score).max
                                                  //> res1: Int = 15862900
}