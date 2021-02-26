package fpfinal.app

import scala.io.StdIn

trait Console {
  val console: Service

  trait Service {
    def readLine(msg: String): String
    def printLine(line: String): Unit
  }
}

trait LiveConsole extends Console {
  override val console: Service = new Service {
    override def readLine(msg: String): String = StdIn.readLine(msg)
    override def printLine(line: String): Unit = println(line)
  }
}
