package functional


object Parsing2v {


  def main(args: Array[String]): Unit = {

    import Reference._

    println(char('a')("a"))
    println(or(char('a'), char('b'))("a"))
    println(or(string("a"), string("b"))("a"))
    println( listOfN(8, or(string("a"), string("b")))("aaabbbaa") )

  }

  type ParsingError = String
  type Parser[A] = String => Either[ParsingError, A]

  object Reference {


    def string(s: String): Parser[String]  = {
      in => {
        if(in == s) Right(in) else Left("does not match")
      }
    }

    def char(c: Char): Parser[Char] = {
      in => {
        val chars = in.toCharArray
        chars match {
          case Array(_) => if (chars(0).equals(c)) Right[ParsingError, Char](c) else Left[ParsingError, Char]("does not match ")
          case _ => Left[ParsingError, Char]("does not match")
        }
      }
    }


    def or[A](p1: Parser[A], p2: Parser[A]): Parser[A] = {
      in => {
        val result = p1(in)
        result match {
          case Right(_) => result;
          case _ => p2(in)
        }
      }
    }

    def listOfN[A](i:Int, parser: Parser[A]): Parser[List[A]] = {

     case class Location(val str:String, val start:Int, val end:Int){
        def isEnd = end > str.length
        def slice = if(!isEnd) str.substring(start, end) else ""
        def nextChar = copy(end=end+1)
        def move = copy(start=end, end=end+1)
      }

      def tryMatch(location: Location, list:List[A]) : List[A] = {
         if(location.isEnd) return list
         parser(location.slice) match {
           case Right(r) => tryMatch(location.move, list ::: List(r))
           case _=> tryMatch(location.nextChar, list)
         }
      }

      in => {
         val res = tryMatch(Location(in, 0, 1), List())
         if( res.length != i ) Left("does not match")
           else Right(res)
      }

    }


  }




  trait Parsers[ParsingError, Parser[+_]] { self =>

    def or[A](p1: Parser[A], p2: Parser[A]): Parser[A]

    //implicit def string(s: String): Parser[String]

    implicit def operators[A](p: Parser[A]) = ParserOps[A](p)

    implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]):
    ParserOps[String] = ParserOps(f(a))

    case class ParserOps[A](p: Parser[A]) {
      def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

      def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    }

  }

}