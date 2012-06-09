import Expression._

class UndefinedException(key: Var) extends Exception {
  def printError = println(key + " is undefined")
}

object StackFrame {
  val stack = new collection.mutable.Stack[StackFrame]
  def addValue(key: Var, v: Object): Unit = stack.head addVal(key, v)
  def addValue(key: String, v: Object): Unit = stack.head addVal(key, v)
  def getValue(key: Var) = stack find (frame => frame hasVal(key)) match {
    case Some(frame) => frame getVal(key)
    case None => throw new UndefinedException(key)
  }
  def startNewFrame = stack push(new StackFrame)
  def stopFrame = stack pop
}

class StackFrame {
  val table = new collection.mutable.HashMap[Var, Object]
  def addVal(key: Var, v: Object): Unit = (table += ((key, v)))
  def addVal(key: String, v: Object): Unit = (table += ((Var(key), v)))
  def hasVal(key: Var) = table isDefinedAt(key)
  def getVal(key: Var) = table get(key) get
}
