/**
 * Represents a statement
 */
abstract class Stmt {
  /**
   * Executes the statement
   * @return Object the result of the statement computation
   */
  def execute: Object
}

/**
 * Represents a function definition
 * @param name the name of the function
 * @param vars the formal parameters of the function
 * @param stmts the statements in the function
 */
case class FuncDef(name: String, vars: List[Var], stmts: StmtList) extends Stmt {
  def execute = {
    val func = new DeclaredFunction(name, vars, stmts)
    StackFrame.addValue(name, func)
    Null
  }
}

/**
 * Represents a statement containing an simple expression
 */
case class ExprStmt(exp: Exp) extends Stmt {
  def execute = exp eval
}

/**
 * Represents a list of statements (as in a block)
 */
case class StmtList(stmtList: List[Stmt]) extends Stmt {
  def execute = {
    def exec(stmtList: List[Stmt]): Object = stmtList match {
      case Nil => Null
      case head::Nil => head execute
      case head::tail => head execute; exec(tail)
    }
    exec(stmtList)
  }
  def isEmpty = stmtList isEmpty
}

case class IfStmt(cond: Condition, stmts: StmtList, elif: List[IfStmt], els: Option[StmtList]) extends Stmt {
  def execute = {
    if(cond.eval == True) { 
      stmts execute
    } else {
      def process(li: List[IfStmt]): Object = li match {
        case Nil => els match {
          case None => Null
          case Some(stmts) => stmts.execute
        }
        case x::xs => {
         if(x.cond.eval == True)
           x.stmts.execute 
         else 
           process(xs)
        }
      }
      process(elif)
    }
  }
}

/**
 * Represents an empty statement (eg: a lineskip)
 */
case object EmptyStmt extends Stmt {
  def execute = Null
}

