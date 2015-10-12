package functional

import java.util.concurrent.ExecutorService

import java.util.concurrent.Future
import functional.Parallelism.Par.Par
import java.util.concurrent._
//import scala.concurrent.Future


object Parallelism {


  def main(args: Array[String]) {


    val es =  Executors.newCachedThreadPool()

    println(  Par.map( Par.sequence[Int](List( sum(IndexedSeq(1,2)), sum(IndexedSeq(3,4)) )))( (list:List[Int]) => list.sum  ).apply(es) )

    val res = sum( IndexedSeq(1,2,3,4,5) ).apply(es)
    println( res.get )

    es.shutdown();
    es.awaitTermination(3, TimeUnit.SECONDS)
  }



  def sum_1(seq: IndexedSeq[Int]) : Int = {
    println(seq)
    if( seq.size <=1 ) seq.headOption getOrElse 0
      else {
          val (l,r) = seq.splitAt( seq.length / 2 )
          sum_1(l) + sum_1(r)
      }
  }


  def sum(seq: IndexedSeq[Int]) : Par[Int] ={
    if(seq.size <= 1) Par.unit( seq.headOption getOrElse 0)
    else{
      val (l,r) = seq.splitAt( seq.size / 2 )
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_+_)
    }
  }


  def calcWorlds(list:List[String]) : List[Int] = {

//    def wordCalculator(s:String): Int = {
//       s.split(s).length
//    }
//
//    val v:Par.Par[Int] = Par.async(1);
//
//    val f  = (s:String) => Par.async(wordCalculator(s))

    val listOfPar = list.map[Int]( (s:String) => s.length ).sum

     //Par.sequence[Int]( listOfPar )

    ???

  }


  //case class Par[A](a: A)

  object Par {

    type Par[A] = ExecutorService => Future[A]

    def unit[A](a: =>A):Par[A] = (es:ExecutorService) => UnitFuture(a)

    private case class UnitFuture[A](get: A) extends Future[A] {
      def isDone = true
      def get(timeout: Long, units: TimeUnit) = get
      def isCancelled = false
      def cancel(evenIfRunning: Boolean): Boolean = false
    }

    def run[A](executorService: ExecutorService)(par: Par[A]) : Future[A] = par(executorService)

    def map2[A,B,C](par: Par[A], parb: Par[B])(mapf: (A,B)=>C ) :Par[C] ={
      (es:ExecutorService) =>{
        val a = par(es)
        val b = parb(es)
        UnitFuture(mapf(a.get, b.get))
      }

    }

    def fork[A](a: => Par[A]):Par[A] ={
      (es:ExecutorService) =>{
        es.submit(new Callable[Future[A]] {
          override def call(): Future[A] =  a(es)
        } ).get()
      }
    }


    def async[A](a: =>A) : Par[A] = {
      fork(unit(a))
    }

    def asyncF[A,B](f:A=>B): A=>Par[B] = {
      (a) => {
          async( f(a) )
      }
    }


    def map[A,B](a:Par[A])(f:A=>B):Par[B] ={
      (es:ExecutorService) => {
        UnitFuture( f(a(es).get) )
      }
    }

    def sequence[A](list:List[Par[A]]) : Par[List[A]] = {
      list.foldLeft[Par[List[A]]]( unit(List()) ) ( (accum:Par[List[A]], next:Par[A]) => map2(accum, next) ( (list:List[A], a:A) => list.::(a)  ) )
      //list.foldRight[Par[List[A]]](unit(List()))  (  (h,t) => map2(h,t)(_ :: _))
    }



    def parFilter[A](f: A=>Boolean)(list:List[A]) : Par[List[A]] = {
      //fork(  unit( list.filter(f) )  )
      es =>
      val af = asyncF(f)

      val filteredList = list.filter( a => af(a).apply(es).get  )

        // A => Par[A,Boolean])

      UnitFuture(filteredList)

    }



  }




}
