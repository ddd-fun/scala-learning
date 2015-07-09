package patterns


object Extentions {


  def main(args: Array[String]) {

    val  res = for(shape <- SomeShapeExamples.shapes) yield(shape.area)
    println(res)

    val res2 = for(s <- ThirdShapeExample.areShapes) yield (s.area)
    println(res2)


  }


  object ThirdShapeExample extends MoreAreaShapes{
    val areShapes = List(new Rectangle(4, 3), new Circle(2), new Square(5))
  }


  object SomeShapeExamples extends AreaShapes{
    val shapes = List(new Rectangle(2, 3), new  Circle(5))
  }



}



trait PerimeterShapes {

  trait Shape {
    def perimeter:Double
  }

  class Rectangle(width:Double, height:Double) extends Shape {
    def perimeter = 2 * (width + height)
  }

  class Circle(radius:Double) extends Shape {
    def perimeter = 2 * Math.PI * radius
  }

}


trait AreaShapes extends PerimeterShapes{

  trait Shape extends super.Shape{
    def area : Double
  }

  class Rectangle(val weight:Double, val height:Double) extends super.Rectangle(height, weight) with Shape{
    def area = weight * height
  }

  class Circle(radius:Double) extends super.Circle(radius) with Shape{
    def area = Math.PI * radius * radius
  }

}


trait MorePerimeterShapes extends PerimeterShapes{

  class Square(side:Double) extends Shape{
    override def perimeter: Double = side * 4
  }

}


trait MoreAreaShapes extends AreaShapes with MorePerimeterShapes{

  class Square(side:Double) extends super.Square(side) with Shape  {
    override def area: Double = side*side
  }

}