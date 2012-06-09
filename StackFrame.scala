
/**
 * Companion object of the stack frame
 * Contains the program stack and methods to act on it
 */
object StackFrame {
  /**
   * The program stack as a stack of stack frames
   */
  val stack = new collection.mutable.Stack[StackFrame]
  /**
   * The program last input
   */
  var lastInput: Stmt = EmptyStmt
  /**
   * Adds a value (variable or function) to the current stack frame
   * @param key the value name
   * @param v the value to add
   */
  def addValue(key: Var, v: Object): Unit = stack.head addVal(key, v)
  def addValue(key: String, v: Object): Unit = stack.head addVal(key, v)
  /**
   * Returns the value corresponding to the key if it exists
   * @param key the value name
   * @throws UndefinedException
   * @return Object
   */
  def getValue(key: Var): Object = stack find (frame => frame hasVal(key)) match {
    case Some(frame) => frame getVal(key)
    case None => throw new UndefinedException(key)
  }
  /**
   * Creates a new stack frame
   */
  def startNewFrame = stack push(new StackFrame)
  /**
   * Deletes the last stack frame
   */
  def stopFrame = stack pop
}

/**
 * Represents a stack frame
 */
class StackFrame {
  /**
   * The variables of the stack frame
   */
  val table = new collection.mutable.HashMap[Var, Object]
  /**
   * Adds a value (variable or function)
   * @param key the value name
   * @param v the value to add
   */
  def addVal(key: Var, v: Object): Unit = (table += ((key, v)))
  def addVal(key: String, v: Object): Unit = (table += ((Var(key), v)))
  /**
   * Checks if the value exists in the stack frame
   * @param key value to check
   * @return Boolean
   */
  def hasVal(key: Var) = table isDefinedAt(key)
  /**
   * Returns the value corresponding to the key. The value must exist
   * @param key the value name
   * @return Object
   */  
  def getVal(key: Var) = table get(key) get
}
