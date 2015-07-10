package functional


object CandyDispenser {

  def main(args: Array[String]) {

    val m = Machine(true, 0, 1)

    val list = List[Input](Coin, Turn, Coin, Turn)


    println(list.reverse.head)



  }


  def transition(input:Input)(machine: Machine) : Machine = {
    (input, machine) match {
      case (_, Machine(_, _, 0)) => machine
      case (Coin, Machine(false, _, _)) => machine
      case (Turn, Machine(true, _, _)) => machine
      case (Coin, Machine(true, coins, candies)) => Machine(false, coins+1, candies)
      case (Turn, Machine(false, coins, candies)) => Machine(true, coins, candies-1)
    }
  }



}


case class StateAction[A,S](runAction:S=>(A,S)){

  def map[B](m: A=>B): StateAction[B,S] = {
    StateAction( s => {val (a,s1) = runAction(s)
      (m(a), s1)
    })
  }


  def flatMap[B](createAction: A=>StateAction[B,S]): StateAction[B,S] = {
    StateAction( s => {val (a,s1) = runAction(s)
                       createAction(a) runAction(s1)
    })
  }


  def sequence(list:List[StateAction[A,S]]):StateAction[List[A], S] ={

   val resultList:List[(A,S)] =  list.foldLeft[List[(A,S)]](List[(A,S)]()) ((l:List[(A,S)],s:StateAction[A,S]) => l ::: List( s.runAction(l.reverse.head._2) ) )

    StateAction( s => (resultList.map( (r:(A,S)) => r._1), resultList.head._2) )
  }


}




sealed trait Input
object Coin extends Input
object Turn extends Input

case class Machine(locked:Boolean, coins:Int, candies:Int)

