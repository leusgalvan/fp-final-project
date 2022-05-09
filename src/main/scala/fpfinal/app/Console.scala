package fpfinal.app

import fpfinal.common.IO

import scala.io.StdIn
import scala.io.AnsiColor

trait Console {
  import Console._

  val console: Service

  trait Service {
    def readLine(msg: String): IO[String]
    def printLine(line: String, level: Level = Info): IO[Unit]
  }
}

trait LiveConsole extends Console {
  import Console._

  override val console: Service = new Service {
    override def readLine(msg: String): IO[String] =
      IO(StdIn.readLine(msg))

    override def printLine(line: String, level: Level = Info): IO[Unit] =
      IO(println(level.getColor + line))
  }
}

object Console {
  sealed trait Level {
    def getColor: String
  }

  case object Info extends Level {
    override def getColor: String = AnsiColor.BLACK
  }

  case object Error extends Level {
    override def getColor: String = AnsiColor.RED
  }

  case object Success extends Level {
    override def getColor: String = AnsiColor.GREEN
  }
}