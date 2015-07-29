package functional


object Parsing {


  def main(args: Array[String]) {


    val res = Reference.char('a').apply("a")

    println(res)
    



  }

  case class ParserError(msg:String)

  type Parser[A] = String => Either[ParserError, A]


  object Reference extends Parsers{
    override def run[A](parser: Parser[A])(str: String): Either[ParserError,A]  = parser(str)

    override def char(c:Char) : Parser[Char] = str => if( str.toCharArray.size == 1 && str.charAt(0) == c ) Right(c) else Left(ParserError("expected "+c))

  }





  trait Parsers{ self=>

    def run[A](parser: Parser[A])(str:String) : Either[ParserError, A]

    def char(c:Char):Parser[Char]

  }

}
