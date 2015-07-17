package functional

import java.lang.Thread


object Parallelism {


  def main(args: Array[String]) {

    println( sum( IndexedSeq(1,2,3,4,0) ) )

  }



  def sum(seq: IndexedSeq[Int]) : Par[Int] ={
    if(seq.size <= 1) Par.unit( seq.headOption getOrElse 0)
    else{
      val (l,r) = seq.splitAt( seq.size / 2 )
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_+_)
    }
  }


  case class Par[A](a: A)

  object Par {

    def unit[A](a: =>A):Par[A] = Par(a)

    def get[A](par: Par[A]) : A = par.a

    def map2[A,B,C](par: Par[A], parb: Par[B])(mapf: (A,B)=>C ) :Par[C] ={
      Par.unit( mapf( Par.get(par), Par.get(parb) ) )
    }

    def fork[A](a: => Par[A]):Par[A] ={
      a
    }

  }




}
