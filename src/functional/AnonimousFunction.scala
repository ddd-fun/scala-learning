package functional


object AnonimousFunction {


  def main(args: Array[String]) {

    println( formatResult("inc2", 1, (x:Int)=>x+2 ) )

    println( formatResult("inc3", 1, x => x+3 ) ) //infers the type of x

    println( formatResult("inc4", 1, (x) => x+4 ) )

    println( formatResult("inc1", 1, _+1 )  )

    println( isSorted[Int]( Array(1,2,3,1), (a,b) => a<b ) )

    println(partial1(1, (a:Int, b:Int)=> a+b)(2))



  }


  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }


  def isSorted[A](arr: Array[A], c: (A,A)=>Boolean) = {
    var result = true
    for(index <- 1 until arr.size){
       if( ! c(arr(index-1), arr(index)) ) result = false
    }
    result
  }

  def partial1[A,B,C](a:A, f:(A,B)=>C) : B=>C ={
    (b:B) =>  f(a,b)
  }



}
