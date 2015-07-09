package recipe.app


trait SimpleReceipes {

  this:SimpleFoods =>

  object FruitSalad extends Receipe(
    "fruit salad",
    List(Apple, Pear),
    "mix all together")


  def allReceipes = List(FruitSalad)

}
