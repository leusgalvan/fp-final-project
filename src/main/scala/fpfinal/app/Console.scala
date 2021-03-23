package fpfinal.app

import fpfinal.common.IO

import scala.io.StdIn

trait Console {
  val console: Service

  trait Service {
    def readLine(msg: String): IO[String]
    def printLine(line: String): IO[Unit]
  }
}

trait LiveConsole extends Console {
  override val console: Service = new Service {
    override def readLine(msg: String): IO[String] =
      IO(StdIn.readLine(msg))

    override def printLine(line: String): IO[Unit] =
      IO(println(line))
  }
}
