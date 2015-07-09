package recipe.app


 object ReceipeApp{

   def main(args: Array[String]) {
     val res = SimpleBrowser.receipesUsing(Apple)

     println(res)


   }

 }


 class Food(val name:String){
   override def toString = name
 }

 class Receipe(val name:String, val ingridients:List[Food], val instaruction:String)  {
   override def toString = name
 }

 object Apple extends Food("Apple")

 object Orange extends Food("Orange")

 object Creme extends Food("Creme")

 object Sugar extends Food("Sugar")


 object FruitSaald extends Receipe(
   "fruit salad",
   List(Apple, Orange, Creme),
   "mix all together"
 )


 object VerySimpleDatabase {

   val foods = List(Apple, Orange, Creme )

   val receipes:List[Receipe] = List(FruitSaald)

   def foodByName(food: String): List[Food] = {
      foods.filter(_.name == food)
   }

   class FoodCategory(val name:String, val foods:List[Food])

   private val cotegoryList = List( new FoodCategory("fruits", List(Apple, Orange)), new FoodCategory("misc", List(Creme, Sugar)) )

   def categories = cotegoryList;

   def allReceipes = receipes



 }


object SimpleBrowser{

  def receipesUsing(food: Food) = {
    VerySimpleDatabase.allReceipes.filter( _.ingridients.contains(food))
  }

}






