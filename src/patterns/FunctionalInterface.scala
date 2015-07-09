package patterns


object FunctionalInterface{


  def main(args: Array[String]): Unit ={


    val list = List(new Person("Anri", "Suri"), new Person("Hansi", "Agreeller"))


    val sorted = list.sortBy( s => s.lastName )

    println( list.sortWith( _.firstName < _.firstName ) )

    val aList = List(new Person("Boli", "Hu"), new Person("Anri", "Hu"),  new Person("Jakob", "Ara"))

    println(  aList.sortWith(fullNameSort) )

    val byLastName:(Person, Person) => Int = _.lastName compareTo _.lastName
    val byFirstName:(Person, Person) => Int = _.firstName compareTo _.firstName

    val lastThenFirstName = makeComparator(byLastName, byFirstName)

    println(aList.sortWith(stableSort(lastThenFirstName)))

  }


  def stableSort(f:(Person, Person) => Int) = (p1:Person, p2:Person) => if( f.apply(p1, p2) < 0 ) true else false



  def fullNameSort(p1: Person, p2:Person) = {
      if(p1.lastName != p2.lastName){
         p1.lastName < p2.lastName        
      }else{
        p1.firstName < p2.firstName         
      }
  }


  def makeComparator( comps: ((Person, Person) => Int)*) = {
     (p:Person, other:Person)  =>
       comps.map( cmp => cmp(p, other)).find(_!=0) .getOrElse(0)
  }


}





case class Person(val firstName:String,  val lastName:String)
