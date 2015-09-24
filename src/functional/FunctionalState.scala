package functional


object FunctionalState {



    def main(args: Array[String]) {


      println( sequence2( List( positiveMax(100), positiveMax(1), positiveMax(1000)) )(RNG.simple(12))  )

      val randInt: Rand[Int] = rng => RNG.simple(150).nextInt();

      println( randomString(randInt)((int:Int)=> String.valueOf(int)+"hello"  ) (RNG.simple(150)))

      println(ints(5)(RNG.simple(10)))

      println(positiveMax(5).apply(RNG.simple(2)))

      println( sequence[Int]( List( positiveMax(100), positiveMax(600), positiveMax(5) )).apply(RNG.simple(2)) )


      val list = List(positiveMax(5), positiveMax(10));

      println("sequence+2 = "+sequence_recImpl(list).apply(RNG.simple(100)))



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


  def sequence2[A](randList: List[Rand[A]]):Rand[List[A]] = {
    rng => {
      randList.foldLeft[(List[A], RNG)]( (List(), rng) )( (accumulatingTuple, nextRand) => {val (a, rng) =  nextRand(accumulatingTuple._2); (accumulatingTuple._1.::(a), rng)}  )
    }
  }


  def map2_v2[A,B,C]( randA: Rand[A], randB: Rand[B]) (mapFunc: (A,B)=>C) : Rand[C] = {
      rng => {
       val (a,rnga) =  randA(rng)
       val (b, rngb) = randB(rnga)
        (mapFunc(a,b), rngb)
      }
  }


  def howTo(str:String)(int:Int) : String = ???


  def randomString(rand:Rand[Int])(f:Int=>String) : Rand[String] = {
    map[Int, String](rand)(f)
  }



  def sequence_recImpl[A](list:List[Rand[A]]) : Rand[List[A]] = {

    def go(r:Rand[A], arg: List[Rand[A]]) : Rand[List[A]]={
      if(arg.isEmpty) map(r)(a=>List(a))
      else map2(r)( go(arg.head, arg.tail) )((a,l)=>l ::: List(a))
    }

    go(list.head, list.tail)

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


  def nonNegativeInt(rng:RNG) : (Integer, RNG) = {
    val nextVal =  rng.nextInt();
    val i : Integer = nextVal._1;
    (if (i<0) -(i+1) else i, nextVal._2 )
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


  def map2[A,B,C](a:Rand[A])(b:Rand[B])(m:(A, B) => C): Rand[C] = {
    rng => {
        val aapp = a(rng)
        val bapp = b(aapp._2)
      (m(aapp._1, bapp._1), bapp._2)
    }
  }


  def ints(count:Integer)(rng:RNG):(List[Integer], RNG) = {

    def go(counter: Integer, accumList: List[Integer], nextRng: RNG) : (List[Integer], RNG) ={
        if(counter > 0){
          val next = nextRng.nextInt();
          go(counter -1, accumList.::(next._1), next._2 )
        }else{
          (accumList, nextRng)
        }
    }

    go(count, List(), rng)
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


  def get[A,S](state: State[A,S]) : State[S, S] = new State[S,S]( s => (s,s) )


  def set[S](s:S):State[Unit, S] = new State[Unit, S]( _ => (Unit, s) )


  def modify(f:S=>S):State[Unit,S] = {
    get(this).flatMap( s => set(f(s)) )
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