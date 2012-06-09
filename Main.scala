object Main {
  import Expression._
  import Function._
  import BuiltinFunctions._
  import Parser._
  import Lexer._
  import Statement._
  

  def main(args: Array[String]) = {
    StackFrame.startNewFrame
    builtInFuncList.foreach(fun => StackFrame.addValue(fun.name, fun))
    prompt
  }

  def prompt = {
    val inputStream = io.Source.fromInputStream(System.in).buffered;
    var parser = new Pars(new Lex(inputStream));
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
        case e: ParseException => e.printError
        case e: ArithmeticException => println("division by 0")
        case e: BadTokenException => e.printError
        case e: EvalException => e.printError
        case e: UndefinedException => e.printError
        case e: NumberFormatException => println("overflow")
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
