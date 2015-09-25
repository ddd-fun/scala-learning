package functional


object CandyDispenser {

  def main(args: Array[String]) {

    val m = Machine(true, 0, 1)

    val list = List[Input](Coin, Turn, Coin, Turn)


    println(list.reverse.head)

    println( simulateMachine(list, m) )

  }


  def simulateMachine(input:List[Input], machine: Machine ) : Machine ={

    val transitions = input.map(transition _).map( mf => new StateAction[Unit, Machine]( m => (Unit,  mf(m) ) ))

     val resultState = transitions.foldLeft( new StateAction[Unit, Machine]( m=>(Unit, m) ) )( (p, n) => p.flatMap( _ => n ) )

     resultState.runAction(machine)._2

  }



  def transition(input:Input)(machine: Machine) : Machine = {
    println("case "+ input+ " " +machine)
    (input, machine) match {
      case (_, Machine(_, _, 0)) => machine
      case (Coin, Machine(false, _, _)) => machine
      case (Turn, Machine(true, _, _)) => machine
      case (Coin, Machine(true, coins, candies)) => Machine(false, coins+1, candies)
      case (Turn, Machine(false, coins, candies)) => Machine(true, coins, candies-1)
    }
  }


  def modify[A,S](stateAction: StateAction[A,S])(f: S=>S): StateAction[Unit,S] = {
    stateAction.flatMap[Unit]( _ => StateAction[S,S]( s=> (s, f(s)) ).map( _=>() )  )
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


//  def modify(f: S=>S): StateAction[S, Unit] = {
//     StateAction( s=> { val (a,s1) = runAction(s)
//          (f(s1), ())
//     }  )
//  }


  def modify(stateAction: StateAction[A,S])(f: S=>S): StateAction[Unit,S] = {
     stateAction.flatMap[Unit]( _ => StateAction[S,S]( s=> (s, f(s)) ).map( _=>() )  )
  }

//  def modify2(stateAction: StateAction[A,S])(f: S=>S): StateAction[Unit,S] = {
//    for{
//       s <- stateAction
//       next <- f
//    }yield()
//  }



  def sequence(list:List[StateAction[A,S]]):StateAction[List[A], S] ={

   val resultList:List[(A,S)] =  list.foldLeft[List[(A,S)]](List[(A,S)]()) ((l:List[(A,S)],s:StateAction[A,S]) => l ::: List( s.runAction(l.reverse.head._2) ) )

    StateAction( s => (resultList.map( (r:(A,S)) => r._1), resultList.head._2) )
  }


}




sealed trait Input
object Coin extends Input
object Turn extends Input

case class Machine(locked:Boolean, coins:Int, candies:Int)

