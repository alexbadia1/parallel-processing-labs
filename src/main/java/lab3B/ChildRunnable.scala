package lab3B

class ChildRunnable(val no: Int) extends Runnable {
  override def run(): Unit = {
    println(s"No: $no Thread Id: ${Thread.currentThread().getId}")
  }
}
