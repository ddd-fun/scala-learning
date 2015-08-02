package functional


object IOing{


  def main(args: Array[String]) {
     converter().run
  }



  def printLine(msg:String) : IO[Unit] = new IO[Unit]{def run = println(msg)}

  def readLine():IO[Int] = new IO[Int] {def run = readInt()}


  def converter(): IO[Unit] ={

    for{
      _ <- printLine("Please, enter degree>..")
      d <- readLine().map(f=> farenheitToCelsius(f))
      _ <- printLine("Converted = "+d)
    }yield ()


  }


  def farenheitToCelsius(d:Int) : Double = {
    (d - 32) * 5.0/9.0
  }


  def printWinner(p1:Player, p2:Player): Unit ={

    val  winner = if(p1.score > p2.score ) p1 else p2

    val msg = "The winner is player with scare:"+ winner.score

    println(msg)

  }


  case class Player(score:Int)







  trait IO[+A] { self =>
    def run: A

    def map[B](f: A => B): IO[B] = new IO[B] { def run = f(self.run) }

    def flatMap[B](f: A => IO[B]): IO[B] =  new IO[B] { def run = f(self.run).run }
  }




}
