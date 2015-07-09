package patterns


object Strategy {

  def main(args: Array[String]) {

    val firstNameMatcher = (p:Person) => p.firstName.isDefined

    val fullNameMatcher = (p:Person)=> p match {case Person(Some(fn), Some(mn), Some(ln))=>true case _ => false}

    val fnBucket = bucket(firstNameMatcher)

    val fullNameBucket = bucket(fullNameMatcher)

    val p1 = new Person(Some("Joe"));

    val p2 = new Person(Some("Bob"), Some("Mc"), Some("Lu"))

    val p3 = new Person(None, Some("Mc"), Some("Lu"))

    fnBucket(p1)
    fnBucket(p2)
    println( fnBucket(p3) )

    fullNameBucket(p1)
    fullNameBucket(p2)
    println( fullNameBucket(p3) )

    val persons = List(p1,p2,p3)



  }

  def bucket( matcher:(Person)=>Boolean):(Person)=>Vector[Person] = {
    var validPeople = Vector[Person]()
    (per:Person)=>{
      if(matcher(per)) validPeople = validPeople :+ per
      validPeople
    }
  }

  case class Person(val firstName:Option[String], val middleName:Option[String] = None, val lastName:Option[String] = None)

}







