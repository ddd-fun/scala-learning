package functional

import java.lang.Thread
import java.util.concurrent.ExecutorService

import java.util.concurrent.Future
import functional.Parallelism.Par.Par
import java.util.concurrent._
//import scala.concurrent.Future


object Parallelism {


  def main(args: Array[String]) {


    val res = sum( IndexedSeq(1,2,3,4,0) ).apply( Executors.newCachedThreadPool())

    println( res.get )

  }



  def sum(seq: IndexedSeq[Int]) : Par[Int] ={
    if(seq.size <= 1) Par.unit( seq.headOption getOrElse 0)
    else{
      val (l,r) = seq.splitAt( seq.size / 2 )
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_+_)
    }
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
        es.submit(new Callable[A] {
          override def call(): A = a(es).get
        } )
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
      es => UnitFuture(list.map(p => fork(p)).map(p=>p(es).get))
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
