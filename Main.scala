object Main {
  import BuiltinFunctions._

  def main(args: Array[String]) = {
    StackFrame.startNewFrame
    builtInFuncList.foreach(fun => StackFrame.addValue(fun.name, fun))
    args match {
      case Array() => prompt
      case Array(fileName) => readFile(fileName)
      case _ => {
        println("usage: scala Main [filename]")
        System.exit(1)
      }
    }
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

  def readFile(fileName: String) = {
    val inputStream = io.Source.fromFile(fileName).buffered    
    val parser = new Parser(new Lexer(inputStream))
    try {
      val stmts = parser.parseFile
      stmts execute
    } catch {
      case e: Printable => e.printError
      case e: ArithmeticException => println("division by 0")
      case e: NumberFormatException => println("overflow"); e.printStackTrace
      case e => e.printStackTrace
    }
  }
}
