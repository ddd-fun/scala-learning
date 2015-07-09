package functional

import java.util.regex.{PatternSyntaxException, Pattern, Matcher}


object OptionMonad {


  def main(args: Array[String]) {

   val res =  mkMatcher("bla").map( matches => matches("bla") )
   println(res)

   println(doesMatch("bla", "bla"))

   mkMatcher("bla").flatMap( m1 => mkMatcher("bla-bla").map( m2 => m1("text") && m2("text") ) )


   println(  map2( Some(1), Some(2) )( ( (x:Int, y:Int)=> x+y) ) )

   println(  map2( Some(1), None )( ( (x:Int, y:Int)=> x+y) ) )


   println( sequence(  List(Some(2), Some(3), None, Some(5), None ) ) )



  }

  //exercise 3
  def map2[X,Y,Z](x:Option[X], y:Option[Y])(f:(X,Y)=>Z) : Option[Z]= {
    x.flatMap( x => y.map(y => f(x,y)) )
  }


  //exercise5
  def sequence[A](list:List[Option[A]]) : Option[List[A]] = {

    //two possible implementations
    val fm = list.flatMap( opt =>  opt match {case Some(a) => List(a); case _=> Nil } )

    if(fm.isEmpty) None  else Some(fm)

//
//    val filtered = list.filter( _.isDefined ).map( _.get )
//
//    if(filtered.isEmpty) None  else Some(filtered)
  }


  def doesMatch(pattern:String, str:String) ={
     for{
         p <- mkMatcher(pattern)
     } yield p(str)
  }


  def mkMatcher(p:String) = {
     mkPattern(p).map( p =>  (str:String) =>  p.matcher(str).matches())
  }

  def mkPattern(p:String) : Option[Pattern] = {
    try{
       Some(Pattern.compile(p))
    }catch {
      case e: PatternSyntaxException => None
    }
  }





}
