package functional

import functional.Parsing.{Failure, Success}

import scala.collection.mutable.ListBuffer


object Parsing {


  def main(args: Array[String]) {



    val strParser = Reference.string("abra")
    println(Reference.run(strParser)("abracadabra"))

    val lebeledParser = Reference.label("error parsing simple string:")(strParser)

    println(Reference.run(lebeledParser)("ablabla"))

    val orParser = Reference.or(Reference.string("abra"), Reference.string("cadabra"))
    println(Reference.run(orParser)("cadabrablabla"))


    val manyParser = Reference.map(Reference.many(Reference.or(Reference.string("abra"), Reference.string("cadabra"))))(_.length)
    println(Reference.run(manyParser)("abraabracadabra"))


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


    override def many[A](a:Parser[A]):Parser[List[A]] = {
      in => {
        val list = new collection.mutable.ListBuffer[A]

        def go(p:Parser[A], offset:Int): Result[List[A]] ={
          val pars = p(in.advanceBy(offset))

          pars match {
            case Success(a,n) => list += a; go(p, n+offset)
            case f@Failure(e) => if(list.isEmpty) f else Success(list.toList, offset)
          }

        }

        go(a, 0)
      }
    }

    override def map[A, B](a: Parser[A])(f: (A) => B): Parser[B] = {
      in=>{
          a(in) match {
            case Success(r,n) => Success(f(r),n)
            case f@Failure(err) => f
          }
      }

    }
  }

  trait Result[+A]
  case class Success[+A](get:A, charConsumed:Int) extends Result[A]
  case class Failure(error:String) extends Result[Nothing]


  case class Location(input:String, offset:Int =0){

    def advanceBy(numChars: Int):Location = {
      copy(input, offset = offset + numChars)
    }

    def currentPos: String = input.substring(offset)

  }



  trait Parsers{ self=>

    def run[A](parser: Parser[A])(str:String) : Result[A]

    def string(str:String) : Parser[String]

    def or[A](a:Parser[A], b:Parser[A]):Parser[A]

    def many[A](a:Parser[A]):Parser[List[A]]

    def map[A,B](a:Parser[A])(f:A=>B):Parser[B]

    def flatMap[A,B](a:Parser[A])(f: A=>Parser[B]):Parser[B] = {
      loc => a(loc) match{
        case Success(a,n) => self.run(f(a))(loc.advanceBy(n).input)
        case f@Failure(e) => f
      }
    }

    def label[A](msg:String)(a:Parser[A]) : Parser[A] ={
      loc => {
          a(loc) match {
            case Failure(err) => Failure(msg+" "+err)
            case s => s
          }
      }
    }

  }

}
