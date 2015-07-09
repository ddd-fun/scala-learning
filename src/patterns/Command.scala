package patterns


object Command {


  def main(args:Array[String]): Unit ={

    val log = new CommandLog(Nil)

    log.execute( makeCommand(  new CashRegister(0), 5 ) )

  }



  def makeCommand(cashRegister: CashRegister, amount: Int) = {
    () => {
      println("adding cash " +amount)
      cashRegister.addCash(amount)
    }
  }



 class CommandLog(var log:List[Any]){
   def execute(command: ()=>Unit): Unit ={
     command.apply()
     log = log::(List(command))
   }
 }


 class CashRegister(var totalAmount: Int){
   def addCash(amount:Int): Unit ={
     this.totalAmount += amount
   }

 }



}
