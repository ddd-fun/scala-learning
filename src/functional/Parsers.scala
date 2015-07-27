package functional


object Parsers {


  def main(args: Array[String]) {

     import Parsers._

     "bla-bla" | "bla"




  }


  case class ParserError(msg:String)

  trait Parser[_]


  trait Parsers[ParserError, Parser[+_]]{ self=>

    def run[A](parser: Parser[A])(str:String) : A

    def parse(s:String):Either[ParserError, String]

    def or[A](one:Parser[A], other:Parser[A]):Parser[A]

    implicit def operators[A](p:Parser[A]) = ParserOps(p)

    implicit def string(str:String):Parser[String]

    implicit def asStringParser[A](a:A)(implicit f: A => Parser[String]) : ParserOps[String] = ParserOps( f(a) )


    def listOfN[A](n: Int, p:Parser[A]) : Parser[List[A]]

    def many[A](a:Parser[A]) : Parser[List[A]]

    def map[A,B](a:Parser[A])(f:A=>B):Parser[B]


    case class ParserOps[A](p:Parser[A]) {

      def | [B>:A](other: Parser[B]) : Parser[B] = self.or(p, other)

      def or[B>:A](other: =>Parser[B]) : Parser[B] = self.or(p, other)

    }


  }

}
