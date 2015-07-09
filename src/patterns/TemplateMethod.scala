package patterns


object TemplateMethod {


  def main (args: Array[String]) {

    val tmp = createTemplate( (in:Double) => { in match { case 1 => "A" case 2 => "B" case _ => "xz="+in } }, println _ )
    tmp(1)
    tmp(2)
    tmp(123.456)

  }



  def createTemplate( converter:(Double => String), printer:(String=>Unit) ) : (Double)=>Unit = {
    (in:Double) => {
      printer("printing:"+converter(in))
    }
  }




}
