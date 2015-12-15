package day15

object day15 {
  val pattern = """(\w+): capacity (-?\d+), durability (-?\d+), flavor (-?\d+), texture (-?\d+), calories (-?\d+)""".r
                                                  //> pattern  : scala.util.matching.Regex = (\w+): capacity (-?\d+), durability (
                                                  //| -?\d+), flavor (-?\d+), texture (-?\d+), calories (-?\d+)

  case class Ingredient(name: String, capacity: Int, durability: Int, flavor: Int, texture: Int, calories: Int)

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

    val calories = (for {
      (amount, ingredient) <- recipe
    } yield ingredient.calories * amount).toList.sum

    Math.max(capacity, 0) * Math.max(durability, 0) * Math.max(flavor, 0) * Math.max(texture, 0)
  }                                               //> score: (recipe: List[(Int, day15.day15.Ingredient)])Int
  
 	def totalCalories(recipe: List[(Int, Ingredient)]): Int = {
 		(for {
      (amount, ingredient) <- recipe
    } yield ingredient.calories * amount).toList.sum
 	}                                         //> totalCalories: (recipe: List[(Int, day15.day15.Ingredient)])Int

  val ingredients = common.loadPackets(List("day15", "day15.txt")).map {
    case pattern(name, capacity, durability, flavor, texture, calories) =>
      Ingredient(name, capacity.toInt, durability.toInt, flavor.toInt, texture.toInt, calories.toInt)
  }                                               //> ingredients  : List[day15.day15.Ingredient] = List(Ingredient(Frosting,4,-2
                                                  //| ,0,0,5), Ingredient(Candy,0,5,-1,0,8), Ingredient(Butterscotch,-1,0,5,0,6),
                                                  //|  Ingredient(Sugar,0,0,-2,2,1))

  def combinations(ingredients: List[Ingredient], leftover: Int): List[List[(Int, Ingredient)]] = {
    ingredients match {
      case Nil => List(Nil)
      case i :: is => (for {
        amount <- 0 to leftover
        combination <- combinations(is, leftover - amount)
        val combination2: List[(Int, Ingredient)] = (amount, i) :: combination
      } yield combination2 ).toList
    }
  }                                               //> combinations: (ingredients: List[day15.day15.Ingredient], leftover: Int)Lis
                                                  //| t[List[(Int, day15.day15.Ingredient)]]
  
  combinations(ingredients, 100).filter(r => totalCalories(r) == 500). map(score).max
                                                  //> res0: Int = 15862900

}