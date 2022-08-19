package ox.cads.testing

/** A log to be used by several threads, based on a shared array.
  * Note that logging involves a synchronization event, which will have an 
  * effect upon the caches of threads, thereby possibly missing Java Memory
  * Model-based errors. 
  * @tparam S the type of the sequential specification object. 
  * @tparam C the type of the concurrent object.
  * @param concObj the concurrent object.
  * @param mkInvoke function to create an InvokeEvent.
  * @param mkReturn function to create a new ReturnEvent.
*/

class SharedLog[S,C](
  concObj: C,
  mkInvoke: GenericLog.MkInvokeType[S], mkReturn: GenericLog.MkReturnType)
extends GenericLog[S, C]{

  /** BoundedBuffer used to store Events. */
  private val inQueue =  new scala.collection.mutable.ArrayBuffer[Event]

  // private val outer = this

  /** Add e to inQueue. */
  @inline private def add(e: Event) = synchronized{ inQueue += e } 

  private val outer = this

  /** Internal GenericThreadLog object. */
  class SharedThreadLog(t: Int) extends GenericThreadLog[S,C]{
    /** Log that the thread performs an operation.
      *
      * @tparam A the type of the result of the operation.
      * @tparam B the type of the return of the operation on the sequential
      * object; this will be A for a tester based on an undoable sequential 
      * object, and (A,S) for a tester based on an immutable sequential object.
      *
      * @param concOp the operation on the concurrent datatype.
      * @param msg a string describing the operation, used in debugging output; 
      * semantically different operations should use different strings.  
      * @param seqOp the corresponding operation on the sequential datatype. */
    def log[A,B](concOp: C => A, msg: String, seqOp: S => B) = {
      // log invocation
      val e = mkInvoke(t,msg,seqOp); add(e)
      // perform operation
      val result = concOp(concObj)
      // log return
      val e1 = mkReturn(t, result); add(e1)
      e.ret = e1
    }

    // /** For two-step synchronisation linearisability, log that a thread performs
    //   * the two-step operation. 
    //   * @tparam A the type of the result of the operation.
    //   * @tparam U the type of the return of the first step of the operation on 
    //   * the sequential object; this will be Unit for a tester based on an
    //   * undoable sequential object, and (Unit,S) for a tester based on an
    //   * immutable sequential object.
    //   * @tparam D the type of the return of the second step of the operation on 
    //   * the sequential object; this will be A for a tester based on an
    //   * undoable sequential object, and (A,S) for a tester based on an
    //   * immutable sequential object.  In each case, the A part is expected to
    //   * match the value returned by the concurrent operation.
    //   */
    // def log2[A,U,B](
    //   concOp: C => A, msg: String, seqOp: S => U, seqOpExit: S => B)
    // = {
    //   // log invocation
    //   val e = mkInvoke(t, msg,seqOp); add(e)
    //   // perform operation
    //   val result = concOp(concObj)
    //   // log return
    //   val e1 = mkReturn(t, ()) // ; add(e1)
    //   // log of exit operation
    //   val e2 = mkInvoke(t, msg+"_EXIT", seqOpExit)
    //   // log of return operation
    //   val e3 = mkReturn(t, result)
    //   outer.synchronized{ inQueue += e1; inQueue += e2; inQueue += e3 }
    //   e.ret = e1; e2.ret = e3
    // }
  }

  /** Get a GenericThreadLog object for thread t. */
  def apply(t: Int) = new SharedThreadLog(t)

  /** Get the contents of the log */
  def getLog : Array[Event] = inQueue.toArray

}
  
