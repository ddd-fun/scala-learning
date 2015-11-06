package functional


object Parsing2v {


  def main(args: Array[String]): Unit = {

    import Parser._


     println(string("aaaa")("aaaaaaa"))



//    println(char('a')("a"))
//    println(or(char('a'), char('b'))("a"))
//    println(or(string("a"), string("b"))("a"))
//    println( listOfN(8, or(string("a"), string("b")))("aaabbbaa") )

  }

  type ParsingError = String
  type Parser[A] = String => Either[ParsingError, A]


  object Parser extends Parsers{

    override def or[A](p1: Parser[A], p2: Parser[A]): Parser[A] = {
     (in) => {
        p1(in) match {
            case Right(a) => Right(a);
            case _=> p2(in)
         }
      }
    }

    override implicit def string(s: String): Parser[String] = {
      in => {
        if(in.startsWith(s)) Right(s) else Left("does not match to "+s)
      }
    }

    override implicit def regex(pattern: String): Parser[String] = {
       in => {
         pattern.r.findFirstIn(in) match {
           case Some(s) => Right(s)
           case _=>Left("doesn't match to regexp="+pattern)
         }
       }
    }
  }



  trait Parsers { self =>

    def or[A](p1: Parser[A], p2: Parser[A]): Parser[A]

    //def flatMap[A,B](f: A=>Parser[B]) : Parser[B]

    implicit def regex(pattern: String) : Parser[String]

    implicit def string(s: String): Parser[String]

    implicit def operators[A](p: Parser[A]) = ParserOps[A](p)

    implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]):
    ParserOps[String] = ParserOps(f(a))

    case class ParserOps[A](p: Parser[A]) {
      def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

      def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    }

  }


//
//  object Reference {
//
//
//    def string(s: String): Parser[String]  = {
//      in => {
//        if(in == s) Right(in) else Left("does not match")
//      }
//    }
//
//    def char(c: Char): Parser[Char] = {
//      in => {
//        val chars = in.toCharArray
//        chars match {
//          case Array(_) => if (chars(0).equals(c)) Right[ParsingError, Char](c) else Left[ParsingError, Char]("does not match ")
//          case _ => Left[ParsingError, Char]("does not match")
//        }
//      }
//    }
//
//
//    def or[A](p1: Parser[A], p2: Parser[A]): Parser[A] = {
//      in => {
//        val result = p1(in)
//        result match {
//          case Right(_) => result;
//          case _ => p2(in)
//        }
//      }
//    }
//
//    def listOfN[A](i:Int, parser: Parser[A]): Parser[List[A]] = {
//
//      case class Location(val str:String, val start:Int, val end:Int){
//        def isEnd = end > str.length
//        def slice = if(!isEnd) str.substring(start, end) else ""
//        def nextChar = copy(end=end+1)
//        def move = copy(start=end, end=end+1)
//      }
//
//      def tryMatch(location: Location, list:List[A]) : List[A] = {
//        if(location.isEnd) return list
//        parser(location.slice) match {
//          case Right(r) => tryMatch(location.move, list ::: List(r))
//          case _=> tryMatch(location.nextChar, list)
//        }
//      }
//      in => {
//        val res = tryMatch(Location(in, 0, 1), List())
//        if( res.length != i ) Left("does not match")
//        else Right(res)
//      }
//    }
//
//
//    def slice[A](parser: Parser[A]) : Parser[String] = {
//      in => {
//        parser(in) match {
//          case Right(r) => Right(r.toString)
//          case l@Left(e) => Left(e)
//        }
//      }
//    }
//
//  }
//
//


}