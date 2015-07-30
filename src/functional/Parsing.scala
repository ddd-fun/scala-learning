package functional


object Parsing {


  def main(args: Array[String]) {



    val strParser = Reference.string("abra")
    println(Reference.run(strParser)("abracadabra"))

    val orParser = Reference.or(Reference.string("abra"), Reference.string("cadabra"))
    println(Reference.run(orParser)("cadabrablabla"))


  }

  case class ParserError(msg:String)

  type Parser[A] = Location => Result[A]


  object Reference extends Parsers{
    override def run[A](parser: Parser[A])(str: String):Result[A]  = parser(Location(str))


    override def string(str:String):Parser[String] = (input:Location) => {
      if(input.currentPos.startsWith(str)) Success(str, str.length)
       else Failure("expected "+str)
    }

    override def or[A](a:Parser[A], b:Parser[A]) : Parser[A] = {
      str => {
         val aRun = a(str)
         aRun match {
           case s@Success(_, _) => s
           case Failure(_) => b(str)
         }
      }
    }

  }

  trait Result[+A]
  case class Success[+A](get:A, charConsumed:Int) extends Result[A]
  case class Failure(error:String) extends Result[Nothing]


  case class Location(input:String, offset:Int =0){

    def advanceBy(numChars: Int):Location = {
      var calcOffset = offset + numChars
      if(calcOffset > input.length)  calcOffset = input.length
      copy(input, offset = calcOffset)
    }

    def currentPos: String =
      if (input.length > 1) input.substring(offset)
      else ""
  }



  trait Parsers{ self=>

    def run[A](parser: Parser[A])(str:String) : Result[A]

    def string(str:String) : Parser[String]

    def or[A](a:Parser[A], b:Parser[A]):Parser[A]

  }

}
