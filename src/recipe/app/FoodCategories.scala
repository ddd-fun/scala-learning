package recipe.app



trait FoodCategories {
  case class FoodCategories(val name:String, val foods:List[Food])
  def allCategories: List[FoodCategories]
}
