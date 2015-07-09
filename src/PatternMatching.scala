
object PatternMatching   {


  implicit val str = "Implicit!"

  def main( args : Array[String])  {


    val some : SomeMatch = Op( Var("phrase"), "aga" )

    val res = some match {
      case Var(str) =>  "just variable with="+str
      case Numer(v) => "some num="+v
      case Op(e @ Var("phrase"), "aga")  => "op matches with inObj="+e
      case _ => "fUCK Up  with "+some
    }

    println(res)

  }


  def printStr(implicit str:String)={
    println(str)
  }



}


abstract class SomeMatch
case class Var(v:String) extends SomeMatch
case class Numer(i:Int) extends SomeMatch
case class Op(v:Var, str:String) extends SomeMatch

