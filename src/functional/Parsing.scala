package functional


object Parsing {


  def main(args: Array[String]) {


    val res = Reference.char('a').apply("a")
    println(res)

    val res2 = Reference.string("abracadabra")("abracadabra")
    println(res2)


  }

  case class ParserError(msg:String)

  type Parser[A] = String => Either[ParserError, A]


  object Reference extends Parsers{
    override def run[A](parser: Parser[A])(str: String): Either[ParserError,A]  = parser(str)

    override def char(c:Char) : Parser[Char] = str => if( str.toCharArray.size == 1 && str.charAt(0) == c ) Right(c) else Left(ParserError("expected "+c))

    override def string(str:String):Parser[String] = inString => {
      if(inString == str ) Right(str) else Left(new ParserError("expected string" + str))
    }

  }





  trait Parsers{ self=>

    def run[A](parser: Parser[A])(str:String) : Either[ParserError, A]

    def char(c:Char):Parser[Char]

    def string(str:String) : Parser[String]

  }

}
