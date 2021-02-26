package fpfinal

import fpfinal.app.Console

trait FakeConsole extends Console {
  var linesToRead: List[String]
  var linesWritten: Vector[String] = Vector.empty

  override val console: Service = new Service {
    override def readLine(msg: String): String = {
      val hd = linesToRead.head
      linesToRead = linesToRead.tail
      hd
    }

    override def printLine(line: String): Unit =
      linesWritten = linesWritten :+ line
  }

}
