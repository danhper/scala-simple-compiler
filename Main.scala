object Main {
  import BuiltinFunctions._

  def main(args: Array[String]) = {
    StackFrame.startNewFrame(new StackFrame)
    builtInFuncList.foreach(fun => StackFrame.addValue(fun.name, fun))
    prompt
  }
  
  /**
   * Runs the interactive interpreter
   */
  def prompt = {
    val inputStream = io.Source.fromInputStream(System.in).buffered
    var parser = new Parser(new Lexer(inputStream))
    var running = true
    print(">> ")
    while(running) {
      try {
        val stmt = parser.interactiveParse
        var obj = stmt.execute
        obj match {
          case Null => ()
          case _ => println(obj)
        }
      } catch {
        case e: Printable => e.printError
        case e: ArithmeticException => println("division by 0")
        case e: NumberFormatException => println("overflow"); e.printStackTrace
        case e: EofException => running = false
        case e => e.printStackTrace
      } finally {
        if(running)
          print(">> ")
        else
          println
      }
    }
  }
}
