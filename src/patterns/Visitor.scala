package patterns


object Visitor {

  def main(args: Array[String]) {

    val person: Person = new SimplePerson("Joe", "Du", "4/2", "Eistenstrasse")

    println(person.fullName)

    println(person.address)

    val complexPerson = new ComplexPerson(new Name("Al", "Mafia"), new Address("Ditt", "4"))

    println(complexPerson.fullName)


  }


  trait Person{
    def firstName: String
    def lastName: String
    def fullName:String
    def houseNumber: String
    def street:String
  }

  
  class SimplePerson(val firstName:String, val lastName:String, val houseNumber:String,  val street:String) extends Person{
    override def fullName = firstName + " " +lastName                                                                        
  }
  
  
  implicit class PersonExtention(val person:Person) {
    def address = person.street +" "+ person.houseNumber
  }
  

  class ComplexPerson(val name: Name, val address: Address) extends Person{
    override def firstName: String = name.firstName

    override def lastName: String = name.lastName

    override def fullName: String = name.firstName + " " + name.lastName

    override def houseNumber: String = address.houseNumber

    override def street: String = address.street
  }

  class Address (val street:String, val houseNumber:String)

  class Name (val firstName:String, val lastName:String)

  
  

}
