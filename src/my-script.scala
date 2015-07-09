import java.io.{FileNotFoundException, IOException, FileReader}


object HelloWorld {
  def main(args: Array[String]) {


    val arr =  Array("1", "2", "3")

    for(el <- arr){
      println(el)
    }


    for(index <- 1 until 10 if index%2==0){
         print(index)
    }

    for(index <- 1 until 10; value <- 10 until 20){
      print((index, value))
    }

    println

   val coll = for{ index <- 1 until 5 } yield index
    println(coll)


    try{
      val file = new  FileReader("input.txt")
    }catch {
      case ex : IOException => println("IO exception"+ ex.getMessage)
      case ex : FileNotFoundException => println("file nof" +ex.getMessage)
    }


    val arg = "pepera"

   val matchres = arg match {
      case "sale" => println("wtf")
      case "peper" => println("good")
      case _ => (i :Int) => {i+1}
    }

    println( matchres )


    import scala.util.control.Breaks._

    breakable {
      break
    }





  }


}

