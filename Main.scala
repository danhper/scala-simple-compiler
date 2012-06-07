object Main {
  import Expression._
  import Function._
  import BuiltinFunctions._
  import Parser._
  import Lexer._
  

  def main(args: Array[String]) = {
    BuiltIns.addBuiltin("fact", BuiltinFunctions.fact)
    BuiltIns.addBuiltin("cos", BuiltinFunctions.cos)
    BuiltIns.addBuiltin("sin", BuiltinFunctions.sin)
    BuiltIns.addBuiltin("tan", BuiltinFunctions.tan)
    BuiltIns.addBuiltin("log", BuiltinFunctions.log)
    BuiltIns.addBuiltin("sqrt", BuiltinFunctions.sqrt)
    prompt
  }

  def prompt = {
    var parser = new Pars(new Lex)
    print(">> ")
    for (line <- io.Source.stdin.getLines) {
      try {
        val exp = parser.parse(line)
        println(exp.eval)
      } catch {
          case e: ParseException => e.printError
          case e: ArithmeticException => println("division by 0")
          case e: BadTokenException => e.printError
          case e: EvalException => e.printError
          case e: NumberFormatException => println("overflow")
          case e => e.printStackTrace
      } finally {
        print(">> ")
      }
    }
  }
}