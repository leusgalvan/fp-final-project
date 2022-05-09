package fpfinal.app

import fpfinal.common.IO

import scala.io.StdIn
import scala.io.AnsiColor

/**
 * A simple implementation of a console that writes and reads
 * to and from standard input.
 */
trait Console {
  import Console._

  val console: Service

  trait Service {
    /**
     * Reads a line from standard input.
     *
     * @param msg a message to show in the console before reading the line (such as 'Please enter a name: ')
     */
    def readLine(msg: String): IO[String]

    /**
     * Prints a line to standard output.
     */
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