package lab3B

object ParentWithRunnable extends App {
  val coreNums = Runtime.getRuntime.availableProcessors();
  val children = for (no <- 0 until coreNums) yield {
    val child = new Thread(new ChildRunnable(no))
    child.start()
    child
  }
  children.foreach(thread => thread.join())
}