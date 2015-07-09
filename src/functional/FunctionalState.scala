package functional


object FunctionalState {



    def main(args: Array[String]) {

      println(positiveMax(5).apply(RNG.simple(2)))

      println(doubleInt.apply(RNG.simple(100)))


      println( sequence[Int]( List( positiveMax(100), positiveMax(600), positiveMax(5) )).apply(RNG.simple(2)) )

    }



  def sequence[A](list:List[Rand[A]]) : Rand[List[A]] = {
     rng => {
        // val tarnsform : List[(Int, RNG)] =   list.foldLeft(List(rng.nextInt()))( (p:List[(Int, RNG)], next:Rand[Int]) => p ::: List(next.apply(p.head._2)  ) )
       //this is really imperative
       var curRng = rng
       var result:List[A] = Nil
       for(r <- list){
           val c = r.apply(curRng)
           result = result ::: List(c._1)
           curRng = c._2
       }
      (result, curRng)
     }
  }


  def flatMap[A,B](a: Rand[A])(mapFunc: A => Rand[B]) : Rand[B] ={
    rng => {
          val c: (A, RNG) = a{rng}
         mapFunc(c._1)(c._2)
    }
  }



  def doubleInt : Rand[(Double,Int)] ={
    map2[Int,Int, (Double, Int)]( rng => rng.nextInt())( rng => rng.nextInt() )( (i, i2) =>  (i2.toDouble, i) )
  }



  def positiveMax(n: Int): Rand[Int] = {
    map(rng => (rng.nextInt()))(i =>  ( i.abs + n ) )
  }



  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] = {
     rng => (a, rng)
  }


  def map[A,B](f: Rand[A])(mFunc: A=>B ):Rand[B] = {
    rng => { val a = f(rng)
     (mFunc(a._1), a._2)
    }
  }


  def map2[A,B,C](a:Rand[A])(b:Rand[B])(m:(A, B) => C) : Rand[C] = {
    rng => {
        val aapp = a(rng)
        val bapp = b(aapp._2)
      (m(aapp._1, bapp._1), bapp._2)
    }
  }


}



case class State[A,S](val run: S=>(A,S)) {

  def map[B](inputMap: A=>B):State[B,S] = {
     new State( s =>  {val (a,s2) =  run(s)
                      (inputMap(a),s2)
               } )
  }


  def flatMap[B](inputStateMap: A=>State[B,S]) : State[B,S] = {
    new State(  s => { val  (a,s1) = run(s)
                       val  newState = inputStateMap(a)
                       (newState.run(s1))
              } )
  }

}



trait RNG {
  def nextInt() : (Int, RNG)
}


object RNG {
  def simple(seed: Long): RNG = new RNG {
    def nextInt = {
      val seed2 = (seed*0x5DEECE66DL + 0xBL) &
        ((1L << 48) - 1)
      ((seed2 >>> 16).asInstanceOf[Int],
        simple(seed2))
    }
  }}