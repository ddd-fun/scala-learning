


object FunStuff {


  def main (args: Array[String]) {

    val lengthFun = (str:String) => str.length

    val func = map(lengthFun)

    val functorRes = func(new MyBox[String]("blblblb"))

    println(functorRes)

    val l:List[String] = List("abc", "def", "jkl")

    println( l.flatMap(_.reverse))

    println(l.flatten)


  }


  //Functor
  def map[A,B]( rawFun: A=>B ): MyBox[A]=>MyBox[B] = ( aBox: MyBox[A]) => new MyBox[B](rawFun(aBox.value))

  //Monad
  def flatMap[A, B]( semiRawFunc: A=>MyBox[B]) : MyBox[A]=>MyBox[B] = (aBox: MyBox[A]) => semiRawFunc(aBox.value)

  //Applicative
  def apply[A,B]( myBoxFunc: MyBox[A=>B] ) : MyBox[A]=>MyBox[B] = (aBox: MyBox[A]) => new MyBox( myBoxFunc.value(aBox.value))


}


class MyBox[T](val value:T) {
  override def toString: String = getClass.getName+"@"+value.toString
}

class LogBox[T](val v:T, val msg:String=""){

  def map[B]( f: T=>B ) :LogBox[B] = new LogBox(f(v), msg)

  def flatMap[B](f: T=>LogBox[B]) : LogBox[B] = flattern( map(f) )

  def flattern[B](logBox: LogBox[LogBox[B]]) = new LogBox[B](logBox.v.v, logBox.msg+logBox.v.msg);

}



