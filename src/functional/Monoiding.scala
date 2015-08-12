package functional


object Monoiding {


  def main(args: Array[String]) {

    val intAddition = new Monoid[Int] {
      override def op(a: Int, b: Int): Int = a + b
      override def zero: Int = 0
    }

    val intMultiplication = new Monoid[Int] {
      override def op(a: Int, b: Int): Int =  a * b

      override def zero: Int = 1
    }

    println(intAddition.op(1, 2))

    println(intMultiplication.op(3, 2))


    val list = List("Hello", " ", "word")

    println(list.foldLeft(StringMonoid.zero)(StringMonoid.op))

    println(list.foldRight(StringMonoid.zero)(StringMonoid.op))


    val part1 = Part("Hello", 1, "dea")
    val part2 = Part("r", 2, "frend")
    println(MonoidicWC.op(part1, part2))

  }



  trait WC
  case class Stub(char:String) extends WC
  case class Part(lStub:String, count:Int, rStub:String) extends WC

  object MonoidicWC extends Monoid[WC]{
    override def op(a: WC, b: WC): WC = {
       (a, b) match {
        case (Part(r1, count1, l1), Part(r2, count2, l2)) => {
           val merge = if ( (l1 + r2).isEmpty )  0 else 1
           Part(r1, count1+count2+merge, l2)
        }
      }

    }

    override def zero: WC = Stub("")
  }



  object StringMonoid extends Monoid[String]{
    override def op(a: String, b: String) = a + b
    override def zero: String = ""
  }


  class OptMonoid[A] extends Monoid[Option[A]]{

    override def op(a: Option[A], b: Option[A]): Option[A] = {
       a orElse b
    }

    override def zero: Option[A] = None
  }


  case class StringMonoid(val v:String) extends Monoid[String]{
    override  def op(a:String, b:String) = a + b
    override def zero = ""
  }


  trait Monoid[A]{
    def op(a:A, b:A) : A
    def zero : A
  }

}
