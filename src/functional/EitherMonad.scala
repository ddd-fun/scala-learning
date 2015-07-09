package functional


object EitherMonad {


  def main(args: Array[String]) {

    println("mean = " + mean( List(1.0, 2.0, 5.0, 1.0) ))

    println("mean = " + mean( List() ))

    println("div = " + div( 4, 2 ))

    println("div = " + div( 4, 0 ))


    val r:Either[Exception, Int] =  Right(25)

    val fres = for{ age <- new MyRight(31)
         name <- new MyRight("Joe")
         salary <- new MyRight(1000000) } yield ( (age, name, salary) )

    println(fres)


  }


  def sequence[L, R](a: List[Either[L,R]]): Either[List[L], List[R]] = {
   ???
  }

  def mean(seq:Seq[Double]) : Either[String, Double] ={
    if(seq.isEmpty) Left("mean of empty list")
     else Right( seq.fold(0.0)(_+_) / seq.size )
  }

  def div(one:Double, other:Double) :Either[Exception, Double] = {
    try{
       Right(one/other)
    }catch {
      case e: Exception => Left(e)
    }
  }


}


trait MyEither[+E, +A] {
  def map[B](f: A => B): MyEither[E, B];
  def flatMap[EE >: E, B](f: A => MyEither[EE, B]): MyEither[EE, B]
  def orElse[EE >: E,B >: A](b: => MyEither[EE, B]): MyEither[EE, B]
  def map2[EE >: E, B, C](b: MyEither[EE, B])(f: (A, B) => C):
  MyEither[EE, C]
}

case class MyRight[+A](val v:A) extends MyEither[Nothing, A] {

  override def toString: String =  "MyRight("+ v + ")"

  override def map[B](f: (A) => B): MyEither[Nothing, B] = {
     new MyRight(f(v))
  }

  override def map2[EE >: Nothing, B, C](b: MyEither[EE, B])(f: (A, B) => C): MyEither[EE, C] = {
     b.flatMap( (b:B) => map((a:A) => f(a,b)   ) )
  }

  override def flatMap[EE >: Nothing, B](f: (A) => MyEither[EE, B]): MyEither[EE, B] = {
      f(v)
  }

  override def orElse[EE >: Nothing, B >: A](b: => MyEither[EE, B]): MyEither[EE, B] = {
    ???
  }
}

case class MyLeft[+E](val v:E) extends MyEither[E, Nothing] {


  override def toString: String =  "MyLeft("+ v + ")"

  override def map[B](f: (Nothing) => B): MyEither[E, B] = {
     this
  }

  override def map2[EE >: E, B, C](b: MyEither[EE, B])(f: (Nothing, B) => C): MyEither[EE, C] = {
    this
  }

  override def flatMap[EE >: E, B](f: (Nothing) => MyEither[EE, B]): MyEither[EE, B] = {
    this
  }

  override def orElse[EE >: E, B >: Nothing](b: => MyEither[EE, B]): MyEither[EE, B] = ???
}