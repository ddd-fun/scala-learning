package functional


object Parsing2v {


  def main(args: Array[String]): Unit = {

    println( char('a')("a") )
    println( or(char('a'), char('b'))("a") )


  }

  type ParsingError = String
  type Parser[A] = String => Either[ParsingError, A]


  def char(c:Char): Parser[Char] = {
    in => {
      val chars = in.toCharArray
       chars match {
         case Array(_) =>  if( chars(0).equals(c) ) Right[ParsingError, Char](c) else Left[ParsingError, Char]("does not match ")
         case _ => Left[ParsingError, Char]("does not match")
       }
    }
  }


  def or[A](p1: Parser[A], p2: Parser[A]) : Parser[A] = {
    in => {
      val result = p1(in)
      result match  {
        case Right(_) => result;
        case _ => p2(in)
      }
    }
  }



  }
