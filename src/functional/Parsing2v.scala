package functional


object Parsing2v {


  def main(args: Array[String]): Unit = {

    import Reference._

    println(char('a')("a"))
    println(or(char('a'), char('b'))("a"))


  }

  type ParsingError = String
  type Parser[A] = String => Either[ParsingError, A]

  object Reference {


    def string(s: String): Parser[String]  = {
      in => {
        in match {
          case s => Right(in)
          case _=> Left("does not match")
        }
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