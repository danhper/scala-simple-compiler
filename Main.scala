object Main {
  import Expression._
  import Function._
  import BuiltinFunctions._
  import Parser._
  import Lexer._
  import Statement._
  

  def main(args: Array[String]) = {
    builtInFuncList.foreach(fun => BuiltIns.addBuiltin(fun.name, fun))
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
        stmt match {
          case ExprStmt(exp) => println(exp.eval)
          case EmptyStmt => ()
          case FuncDef(name, varList, stmts) => println(name + ' ' + varList + ' ' + stmts)
          case _ => ()
        }

      } catch {
        case e: ParseException => e.printError
        case e: ArithmeticException => println("division by 0")
        case e: BadTokenException => e.printError
        case e: EvalException => e.printError
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
