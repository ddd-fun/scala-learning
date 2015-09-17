package recipe


package object app {


  def main(args: Array[String]) {


      val mail = "ME@dot.com"

      mail match {
       case  Email( name @ UpperCase(), domain) => println("hre is mail - "+name+"  "+ domain)
       case  _ => println("no mail")
      }


  }

  abstract case class Food(val name:String)
  case object Apple extends Food("apple")
  case object Orange extends Food("orange")


  abstract class Database{

    def findFood(name:String): Option[Food]

  }

  abstract class Browser{

    val database : Database

    def findFood(name:String):Option[Food] =  database.findFood(name)

  }


  object FruitDatabase extends Database{
    val foods = List(Apple, Orange)
    override def findFood(name: String): Option[Food] = foods.find(_.name == name)
  }


   object FruitBrowser extends Browser{
     val database = FruitDatabase
   }



  object Email{
    def unapply(str:String): Option[(String, String)] ={
       val arr: Array[String] = str.split("@")
       if(arr.size == 2){
         Some((arr(0), arr(1)))
       }else{None}
    }
  }

  object UpperCase{
    def unapply(str: String) : Boolean ={
       str.toUpperCase == str
    }
  }



}
