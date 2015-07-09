package recipe.app


abstract class Database extends FoodCategories{

   def allFoods:List[Food]
   def allReceipes: List[Receipe]
   def foodName(name:String) = {
     allFoods.find(_.name == name)
   }

}
