

object MyObj{

  def main( args : Array[String])  {
    val list = List[String]("1", "2", "3");

    val listCons = "apple" :: ("oranges" :: ("kiwi" :: Nil))


    println(listCons ::: list)



  }

  def addInts(one : Int, other : Int) : Int = {
    one+other;
  }


  var factor = 3;

  var multiplier =  (one : Int ) =>  {
     one * factor;
  }


}

