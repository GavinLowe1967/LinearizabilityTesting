package ox.cads.testing

// import ox.cads.util.Profiler
import scala.collection.mutable.ArrayBuffer
// import scala.collection.mutable.Undoable

/** A tester for linearizabilty.
  * @tparam S the type of the sequential specification object.
  * @tparam C the type of the concurrent datatype.
  * @tparam L the type of log to be used.
  * @param mkLog a function to produce a log, given functions that produce 
  *   invocation and return events.
  * @param worker a function that produces a worker, given its identity and a 
  *   log to use.
  * @param p the number of workers.
  * @param concObj the concurrent object to use.
  * @param seqObj the sequential specification object.
  * @param solver the generic solver to use. */ 
class LinearizabilityTester[S, C, L <: GenericLog[S,C]](
  mkLog: (GenericLog.MkInvokeType[S], GenericLog.MkReturnType) => L,
  worker: LinearizabilityTester.WorkerType[S,C],
  p: Int, concObj: C, seqObj: S, solver: GenericSolver[S, Event])
{
  /* Note: the following duplicates code in ox.cads.util.ThreadUtil.scala, but
   * makes the linearzability testing independent. */

  /** Create a thread that performs comp */
  private def mkThread(comp: => Unit) : Thread = 
    new Thread(new Runnable{ def run = comp })

  /** Create a system of processes `proc(i)` for `i <- [0..p)`; run that system,
    * terminating when all the processes terminate, but if any thread throws
    * an exception, then throw an exception. */
  private def runIndexedSystemStrict(p: Int, proc: Int => Unit) = {
    var done = new java.util.concurrent.atomic.AtomicInteger
    val threads = Array.tabulate(p)(i => mkThread{proc(i); done.getAndIncrement})
    threads.foreach(_.start)
    threads.foreach(_.join)
    assert(done.get == p)
  }

  /** Run the tester.
    * @return a result as defined in Solver. */ 
  def apply() : Int = {
    // Make log
    val log : L = mkLog(solver.mkInvoke, solver.mkReturn)
    // Run workers
    runIndexedSystemStrict(p, i => worker(i, log(i)))
    // Test for linearizability
    solver.solve(log.getLog)
  }
}

// --------- Companion object ---------

object LinearizabilityTester{

  /** Type of functions that produce workers. */
  type WorkerType[S,C] = (Int, GenericThreadLog[S, C]) => Unit

  /** Make the log to use. */
  private def mkLog[S, C](tsLog: Boolean, p: Int, concObj: C)(
      mkInvoke: GenericLog.MkInvokeType[S], mkReturn: GenericLog.MkReturnType)
      : GenericLog[S,C]
  = if(tsLog) new TSLog[S,C](p, concObj, mkInvoke, mkReturn)
    else new SharedLog[S,C](concObj, mkInvoke, mkReturn)

  // --------- Factory methods ---------

  /** Produce a linearizability tester based on the Wing & Gong Graph Search
    * Algorithm and a shared log. 
    * @tparam S the type of the sequential specification datatype.
    * @tparam C the type of concurrent datatypes.   
    * @param seqObj the sequential specification datatype. 
    * @param concObj the concurrent object. 
    * @param p the number of threads.
    * @param worker a function that produces a worker.
    * @param tsLog should a timestamp-based log be used? */
  def WGGraph[S, C](
      seqObj: S, concObj: C, p: Int, worker: WorkerType[S,C], 
      tsLog: Boolean = true, maxSize: Long = -1) 
  = new LinearizabilityTester[S, C, GenericLog[S,C]](
      mkLog(tsLog, p, concObj), worker, p, 
      concObj, seqObj, new WGGraph[S](seqObj, p, maxSize)
    )

  /** Produce a linearizability tester based on the Wing & Gong Tree Search
    * Algorithm and a shared log. 
    * @tparam S the type of the sequential specification datatype.
    * @tparam C the type of concurrent datatypes.   
    * @param seqObj the sequential specification datatype. 
    * @param concObj the concurrent object. 
    * @param p the number of threads.
    * @param worker a function that produces a worker.
    * @param tsLog should a timestamp-based log be used? */
  def WGTree[S <: Undoable, C](
      seqObj: S, concObj: C, p: Int, worker: WorkerType[S,C], 
      tsLog: Boolean = true, maxSize: Long = -1) 
  = new LinearizabilityTester[S, C, GenericLog[S,C]](
      mkLog(tsLog, p, concObj), worker, p,
      concObj, seqObj, new WGLinearizabilityTester[S](seqObj, p, maxSize)
    )
  
  /** Produce a linearizability tester based on JIT Tree Search and a 
    * shared log. 
    * @tparam S the type of the sequential specification datatype.
    * @tparam C the type of concurrent datatypes.   
    * @param seqObj the sequential specification datatype. 
    * @param concObj the concurrent object. 
    * @param p the number of threads.
    * @param worker a function that produces a worker.
    * @param tsLog should a timestamp-based log be used? */
  def JITTree[S <: Undoable, C](
      seqObj: S, concObj: C, p: Int, worker: WorkerType[S,C], 
      tsLog: Boolean = true, maxSize: Long = -1)
  = new LinearizabilityTester[S, C, GenericLog[S,C]](
      mkLog(tsLog, p, concObj), worker, p, 
      concObj, seqObj, new JITLinUndoTester[S](seqObj, p, maxSize)
    )

  /** Produce a linearizability tester based on JIT Graph Search and a 
    * shared log. 
    * @tparam S the type of the sequential specification datatype.
    * @tparam C the type of concurrent datatypes.   
    * @param seqObj the sequential specification datatype. 
    * @param concObj the concurrent object. 
    * @param p the number of threads.
    * @param worker a function that produces a worker.
    * @param tsLog should a timestamp-based log be used? */
  def JITGraph[S, C](
      seqObj: S, concObj: C, p: Int, worker: WorkerType[S,C], 
      tsLog: Boolean = true, maxSize: Long = -1) 
  = new LinearizabilityTester[S, C, GenericLog[S,C]](
      mkLog(tsLog, p, concObj), worker, p,
      concObj, seqObj, new DFSGraphJITLinTester[S](seqObj, p, maxSize)
    )

  /** Produce a linearizability tester based on breadth-first JIT Graph 
    * Search and a shared log. 
    * @tparam S the type of the sequential specification datatype.
    * @tparam C the type of concurrent datatypes.   
    * @param seqObj the sequential specification datatype. 
    * @param concObj the concurrent object. 
    * @param p the number of threads.
    * @param worker a function that produces a worker.
    * @param tsLog should a timestamp-based log be used? */
  def BFSJIT[S <: AnyRef, C](
      seqObj: S, concObj: C, p: Int, worker: WorkerType[S,C], 
      tsLog: Boolean = true, maxSize: Long = -1)
  = new LinearizabilityTester[S, C, GenericLog[S,C]](
      mkLog(tsLog, p, concObj), worker, p, 
      concObj, seqObj, new BFSJITLinTester[S](seqObj, p, maxSize)
    )

  // TODO: maybe DFSJITLin     
}
