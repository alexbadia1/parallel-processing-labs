package lab3B

class ChildThread(val no: Int) extends Thread {
  override def run(): Unit = {
    println(s"No: $no Thread Id: ${this.getId}")
  }
}
