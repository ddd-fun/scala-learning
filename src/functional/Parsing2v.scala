package functional


object Parsing2v {


  def main(args: Array[String]): Unit = {

      println(char('a').apply("a"))


  }

  type ParsingError = String

  type Parser[A] = String => Either[ParsingError, A]


  def char(c:Char): Parser[Char] = {
    in => {
      val chars = in.toCharArray
       chars match {
         case Array(_) =>  if( chars(0).equals(c) ) Right[ParsingError, Char](c) else Left[ParsingError, Char](in + " does not match " + c)
         case _ => Left[ParsingError, Char](in + " does not match " + c)
       }
    }
  }




  }
