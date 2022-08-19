package ox.cads.testing

import scala.collection.mutable.ArrayBuffer
//import ox.cads.util.Profiler

/** A configuration of the testing automaton in the Linear Tester.
  * @param stateWrapper a wrapper around the state of the sequential object
  * @param tStates information about the states of threads.
  */
class Configuration[S]
  (var stateWrapper: StateWrapper[S], val tStates: ThreadStates[S])
{
  def this(theState: S, tStates: ThreadStates[S]) = 
    this(new StateWrapper(theState), tStates)

  /* Note: objects of this class are mutable (but care must be exercised!).
   * However, the enclosed stateWrapper should be treated as immutable. */

  /** A collection of configurations */
  type Configs = ConfigSet[S]
  type ConfigsAB = ArrayBuffer[Configuration[S]]

  /** The state associated with this */
  private def state = stateWrapper.get

  // ----- Query operations -----

  /** Does t have a pending operation? */
  def hasPending(t: Int) : Boolean = tStates.hasPending(t)

  /** Can t's operation return? */
  def canReturn(t: Int) : Boolean = tStates.allowsReturn(t)

  // ----- Destructive updates -----

  /** Log that op is invoked upon this configuration (destructive). */
  def invoke[A](t: Int, msg: String, op: S => (A, S), result: A) = 
    tStates.invoke(t, msg, op, result) 

  /** Fire op upon this configuration; test whether it produces result; 
    * if so, update this to the resulting configuration and return true 
    * (destructive). */
  private def fireReturn(t: Int, msg: String, op: S => (Any, S), result: Any)
  : Boolean = {
    try{
      val (result1, newStateW) = stateWrapper.doOp(msg, op)
      // println(stateWrapper.get+" -"+t+"."+msg+"-> "+newStateW.get)
      if(result1 == result){
        stateWrapper = newStateW; tStates.logFireReturn(t); true
      }
      else false
    } catch {
      case _ : java.lang.IllegalArgumentException =>
        // This is outside the precondition of the operation in the
        // sequential object
        false
    }
  }

  /** Create new configurations caused by firing any pending operations from
    * this, then logging t returning result; t should be the last fired, or 
    * no operations should be fired if t has already been fired.  Add all 
    * such to configs.  (Destructive.) */
  def logReturnJITLin(t: Int, result: Any, configs: Configs) = {
    if(tStates.hasPending(t)){ 
      // t's op has not yet been fired.  Fire any other operations
      val configs1 = fireAnyExcept(t)
      // Now try firing op on the resulting configs
      val (msg, op, res1) = tStates.getOp(t)
      assert(res1 == result)
      for(c <- configs1){
  	val ok = c.fireReturn(t, msg, op, result)
  	if(ok) configs += c 
      } 
    }
    else if(tStates.allowsReturn(t, result)){ 
      tStates.logReturn1(t, result); configs += this
    }
  } 

  // ----- Non-destructive updates ----- 

  /** Create new configuration corresponding to op being invoked by t.  
    * (Non-destructive.) */
  def logInvoke[A](t: Int, msg: String, op: S => (A, S), result: A) 
  : Configuration[S]= {
    val newTStates = tStates.logInvoke(t, msg, op, result) 
    new Configuration[S](stateWrapper, newTStates)
  }

  /** Fire t's operation; if it produces the expected value, return the
    * resulting configuration; otherwise return null (non-destructive). */
  private def fire1(t: Int) : Configuration[S] = {
    val (msg, op, expectedRes) = tStates.getOp(t)
    try{
      val (result, newStateW) = stateWrapper.doOp(msg, op)
      if(expectedRes == result){
        // println(stateWrapper.get+" -"+t+"."+msg+"-> "+newStateW.get)
        val newTStates = tStates.logFire(t, result)
        new Configuration(newStateW, newTStates)
      }
      else null
    } catch {
      case _ : java.lang.IllegalArgumentException =>
        // This is outside the precondition of the operation in the
        // sequential object
        null
    }
  }
  /** Create all configurations reachable by firing operations from this, 
    * other than the operation of t.  (Non-destructive). */
  private def fireAnyExcept(t: Int) : ConfigsAB = {
    val configs = new ArrayBuffer[Configuration[S]]() 
    fireAnyExcept1(t, configs)
    configs
  }

  /** Create all configurations reachable by firing operations from this, 
    * other than the operation of t, adding them to result.  
    * (Non-destructive). */
  private def fireAnyExcept1(t: Int, result: ConfigsAB) : Unit = {
    result += this
    for(t1 <- 0 until tStates.length; if t1 != t && tStates.hasPending(t1)){
      val newC = fire1(t1); 
      if(newC != null) newC.fireAnyExcept1(t, result)
    }
  }

  /** Try to return t's operation, firing it if it hasn't already been fired
    * and produces the expected value (non-destructive).  
    * Pre: thread t is in either a Pending or Ret state
    * @return Either the new configuration (if successful) or the result 
    * produced (otherwise). */
  def fireRet(t: Int) : Either[Configuration[S],Any] = {
    if(tStates.allowsReturn(t)){
      val newTStates = tStates.logReturn(t)
      Left(new Configuration[S](stateWrapper, newTStates))
    }
    else{ // thread t is pending
      val (msg, op, expectedRes) = tStates.getOp(t)
      try{
        val (result, newStateW) = stateWrapper.doOp(msg, op)
        if(expectedRes == result){
	  // println(stateWrapper.get+" -"+t+"."+msg+"-> "+newStateW.get)
	  val newTStates = tStates.logFireReturn1(t)
	  Left(new Configuration(newStateW, newTStates))
        }
        else Right(result)
      } catch {
        case _ : java.lang.IllegalArgumentException =>
          // This is outside the precondition of the operation in the
          // sequential object
          Right("outside precondition")
      }
    }
  }

  /** Try to fire t's operation if it produces the expected value 
    * (non-destructive). 
    * Pre: t has a pending operation. 
    * @return Either the new configuration (if successful) or the result 
    * produced (otherwise).  */
  def fire(t: Int) : Either[Configuration[S],Any] = {
    val (msg, op, expectedRes) = tStates.getOp(t)
    try{
      val (result, newStateW) = stateWrapper.doOp(msg, op)
      if(expectedRes == result){
        // println(stateWrapper.get+" -"+t+"."+msg+"-> "+newStateW.get)
        val newTStates = tStates.logFire(t, result)
        Left(new Configuration(newStateW, newTStates))
      }
      else Right(result)
    } catch {
      case _ : java.lang.IllegalArgumentException =>
        // This is outside the precondition of the operation in the
        // sequential object
        Right("outside precondition")
    }
  }

  /** Create new configurations caused by firing any pending operations from
    * this, then logging t returning result; t should be the last fired, or 
    * no operations should be fired if t has already been fired.  
    * (Non-destructive.) */
  def logReturnJITLinND(t: Int, result: Any) : ConfigsAB = {
    if(tStates.hasPending(t)){ 
      // t's op has not yet been fired.  Fire any other operations
      val configs1 = fireAnyExcept(t)
      val configs = new ArrayBuffer[Configuration[S]](configs1.length)
      // Now try firing op on the resulting configs
      val (msg, op, res1) = tStates.getOp(t)
      assert(res1 == result)
      for(c <- configs1){
  	val ok = c.fireReturn(t, msg, op, result)
  	if(ok) configs += c 
      } 
      configs
    }
    else if(tStates.allowsReturn(t, result)){
      // t's op has already been fired and gave result
      val newTStates = tStates.logReturn(t, result)
      val newConfig = new Configuration[S](stateWrapper, newTStates)
      val configs = new ArrayBuffer[Configuration[S]](1)
      configs += newConfig
    }
    else // t's op has already been fired but gave different result
      new ArrayBuffer[Configuration[S]](0) 
  }

  // ----- Misc operations -----

  override def toString = "Configuration("+state+" , "+tStates+")"

  override def equals(other: Any) = other match{
    case (c:Configuration[S @unchecked]) => 
      tStates == c.tStates && stateWrapper == c.stateWrapper
  }

  override def hashCode = tStates.hashCode + stateWrapper.hashCode
}
