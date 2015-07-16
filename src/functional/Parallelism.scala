package functional


object Parallelism {


  def main(args: Array[String]) {

    println( sum( IndexedSeq(1,2,3,4,0) ) )

  }



  def sum(seq: IndexedSeq[Int]) : Int ={
    if(seq.size <= 1) seq.headOption getOrElse 0
    else{
      val (l,r) = seq.splitAt( seq.size / 2 )
      sum(l) + sum(r)
    }
  }


  case class Par[A](a: =>A) {

    def unit[A](a: =>A):Par[A] = Par(a)

    def get(par: Par[A]) : A = a

  }





}
