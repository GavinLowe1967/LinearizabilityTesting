package ox.cads.testing

/** A log to be used by several threads, based on timestamping.
  * @tparam S the type of the sequential specification object. 
  * @tparam C the type of the concurrent object. 
  * @param p the number of threads.
  * @param concObj the concurrent object.
  * @param mkInvoke function to create an InvokeEvent.
  * @param mkReturn function to create a new ReturnEvent.
*/
class TSLog[S,C](
  p: Int, concObj: C,
  mkInvoke: GenericLog.MkInvokeType[S], mkReturn: GenericLog.MkReturnType)
extends GenericLog[S,C]{
  TSLog.checkOS

  /** Array holding the individual logs. */
  private val logs = 
    Array.tabulate(p)(t => 
      new TSThreadLog[S,C](t, concObj, mkInvoke, mkReturn))

  /** Get an individual log for thread t to use. */
  def apply(t: Int) : TSThreadLog[S,C] = logs(t)

  /** Get the logs, as a single array. */
  def getLog : Array[Event] = TS.merge(logs.map(_.get))
}
		  
// ==================================================================

/** Companion object. */
object TSLog{
  /** Has checkOS been called previously? */
  private var givenWarning = false

  /** Give a warning if the operating system is a variant of Windows.  Called
    * when an Log object is created. */
  def checkOS = if(!givenWarning){
    val osName = System.getProperty("os.name").map(_.toLower)
    val pattern = "windows"
    // println(osName)
    var i = 0; var found = false
    while(i+pattern.length <= osName.length && !found){
      // Test if pattern appears in osName starting from index i
      var j = 0
      // Inv: osName[i..i+j) = pattern[0..j)
      while(j < pattern.length && osName(i+j) == pattern(j)) j += 1
      found = j == pattern.length; i += 1
    }
    if(found) println(
      "Warning: You seem to be running a version of Windows.  However,\n"+
        "objects from debug.Log may not work correctly on such operating\n"+
        "systems, because of the timestamping mechanism.  It is recommended\n"+
        "that you use an instance of debug.SharedLog instead.")
    givenWarning = true
  }
}
