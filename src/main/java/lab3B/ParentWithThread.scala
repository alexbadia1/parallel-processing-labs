package lab3B

object ParentWithThread extends App {
  val coreNums = Runtime.getRuntime.availableProcessors()
  val child = new Thread(new ChildThread(0))
  child.start()
  val numThreads = Thread.activeCount()
  println(s"Cores: $coreNums Active Threads: $numThreads Thread Id: ${Thread.currentThread().getId}")
  child.join()

}
